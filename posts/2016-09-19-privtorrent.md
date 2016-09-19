------
title: Privately send data to host without disclosing IP
summary: Distributed network connections :)
tags: algorithm, networks
category: networks
id: dist-private-communication
author: Saksham Sharma
------

While thinking on some lines related to networks and security, I recently came up with an idea of having a distributed network connection without disclosing your identity to the sender. The idea is still nascent, and requires you to have root access to your machine (and a custom kernel module, which I'm miles away from writing in the middle of my semester). I'd nevertheless appreciate comments on this idea :)

### TCP
A brief look at the relevant parts of TCP/IP needed for this:

* The sender opens a TCP connection with the client. This connection is over IP packets, which are stateless.
* IP can be spoofed easily. It is possible to send a packet to someone which has a misleading 'source' header set in the IP packet.
* Spoofing your identity over a TCP connection is not easy, since the response is sent to the host with the IP you set in the source.
* TCP prevents someone else from sending packets as you by setting up a random sequence number initially during the handshaking with the client.

### Basic idea

I'll take the example of torrent here, and assume that there are `n` senders (peers) in the torrent connection, who are uploading, let's say, ubuntu's iso file to a host `cl` (client); and the senders don't want their IP to be known to `cl`. I will not attempt to get into the technical details of torrent here though.

Imagine that there is a service like tor which let's you talk to hosts anonymously. This would be somewhat of a proxy server. Call it `p`.

A possible way would be for all the `n` senders to route their traffic through `p`. This is not a scalable idea perhaps, since many senders sending data via `p` would consume a lot of network resources over `p`.

Here is a possibly better way. Host `p` sets up a TCP connection with the client `cl`. It gets a certain receive buffer. The `n` senders decide among themselves about how they wish to communicate the data to the client. One way is for each sender to send every `n`th byte. So sender `i` sends the `i`th, `(i+1)`th and so on bytes. The host `p` lets all the senders know the relevant TCP packet numbers. Now the host `p` only has to communicate with the client `cl` mostly, receiving it's responses on the TCP connection it set up. All the senders spoof their packets by sending the packets with the source marked as `p`. The client will get the packets in the right order (due the senders deciding, and using the right TCP sequence number), and will respond to `p`, which can later inspect the response and let the senders know accordingly if it is something they should know.

### Details

* Senders need to honor the receive window of the client. Perhaps each one treats the effective receive window as the `1/n`th fraction of the actual receive window.
* There is no need to send every ACK to the senders, except to help with fast retransmits, and for them to know the network state. There might exist ways to avoid sending all responses from `p` to the senders (which puts load on `p`).
* If the network has low packet loss rate, it *may* be advantageous to not communicate all ACKs to the senders. It would only slow down responses when there's a packet loss.
* If the host `p` detects a dropped packet, it communicates that to the sender who was supposed to send that packet.
* If host `p` detects network congestion (triple duplicate ACK, or ECN etc), it communicates this with all the senders.

### Advantages

* The client `cl` cannot find out which IP sent the actual packet.
* You do not need as high network resources as tor on the host machine `p` for this to work.
* Possibly inspire ideas related to private distributed network communication.
* One could possibly send part data from one host, and part from the other host via this mechanism. Thus, someone doing a man-in-the-middle on one sender would only see partial data being sent. So you could open a connection to a website, and a listener would see you sending `GET / HTTP/1.0` to that website, while there would be another machine which sent the remaining request (maybe some password in clear-text, which was mandated by the website. I know, not a good example).
