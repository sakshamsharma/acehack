------
title: Chrome and Firefox resource usage experiment
technical: true
summary: Finally put them to the test!
author: Saksham Sharma
image: http://media2.giga.de/2015/05/chrome-vs-firefox2-rcm992x0.jpg
tags: technical, browser, experiment
id: chrome-vs-ffx
category: experiments
------

## Browser WAR!
Why bother fighting over features in browsers when you can decide using some hard facts?

I had recently managed to make Firefox work for me (it still isn't as fluid as Chrome, but is now close, thanks to using the beta version which has e10s support). Still confused about whether its really better than Chrome, I decided to put them to the test!

## Results first, because.. well.
Since majority would come here for the results, well...

***drum roll***

Firefox wins *hands down* here. Personally, this was not a surprise.

Chrome was always know to be a RAM hog, but it also seems to have a huge lot more power usage as compared to Firefox as well.

**Update** I'm back to Chrome these days. I noticed it works much more smoother, and hangs much less often.

### RAM
Not a new thing, but yes, Firefox uses lower RAM than Chrome *consistently*. For description of the actions during the course of the benchmark, look at the next section.

<img src="/images/articles/ram-plot.png" style="width: 95%; height: auto;"></img>

> Don't laugh at the plot :P
>
> Also, the higher one is Chrome, although it's not apparent.
>
> Bad gnuplot skills ^

### Power
I measured the power output of my laptop's battery while doing various actions on both browsers, and Firefox again demonstrates much lower usage. Really good for people who keep running out of battery and complain they cannot figure out the reason (Yes, I'm also talking about myself). Firefox, good job there!

<img src="/images/articles/power-plot.png" style="width: 95%; height: auto;"></img>

## The experiment
I designed a casual benchmark for testing the browsers' resource usage.

Here's the timeline of the benchmark:

| Start Time   | Duration of step | Action |
|:------------:|:----------------:|--------|
| 0 seconds    | 25 seconds       | Startup of browser |
| 25 seconds   | 20 seconds       | Open Viva La Vida video on Youtube |
| 45 seconds   | 20 seconds       | Open Instant Crush video on Youtube in a new tab |
| 65 seconds   | 20 seconds       | Open The Scientist video on Youtube in a new tab |
| 85 seconds   | 20 seconds       | Open Facebook in a new tab |
| 105 seconds  | 20 seconds       | Close all tabs but keep browser open |

**NOTE** All videos were running simultaneously by 85 seconds. No tab was closed prior to 105 seconds.

I executed these exact actions on both browsers, taking care to start with exactly the same conditions in both experiments.

Things done to ensure a clean test environment:

* A fresh boot in both cases.
* No other applications were running, except the light weight window manager (XMonad) and core services.
* Browser was not loaded any time before the start of the test.
* The battery level (95%) and the battery usage rate (power) were same before the start in both cases.
* All plugins (except ad-blocker) were disabled in both browsers prior to the start of the experiment.
* The browsers used were the publicly available binaries, not custom compiled.
* Both browsers did not use dedicated GPU for any purpose (nvidia module was not loaded).
* Yes, both browsers had the same volume set on system as well as Youtube.

**NOTE** I left ad-blocker running in both the browsers since Ads would just introduce variability and delays in video starts.

Here are the links used for the experiment:

* [Viva La Vida](https://www.youtube.com/watch?v=dvgZkm1xWPE)
* [Instant Crush](https://www.youtube.com/watch?v=a5uQMwRMHcs)
* [The Scientist](https://www.youtube.com/watch?v=RB-RcX5DS5A)

The experiments were conducted on a Lenovo Y50-70 with a 4 cell (and dismal) battery, 8 GB RAM, and an i7 4705HQ Processor.

## Scripts used

#### Data recording
Here is the script used to record the data. Feel free to test it on yours as well.

``` bash
#!/bin/bash

if [ -z "$1" ]
then
    echo "CUSTOM MESSAGE"
    echo "Arguement not supplied"
    echo "Usage: ./test.sh <file_1> <file_2"
    exit
fi

for i in {1..125}
do
  echo $i
  power=$(cat /sys/class/power_supply/BAT1/power_now)
  ramFree=$(cat /proc/meminfo | grep 'MemFree:\ *[0-9]*' | grep '[0-9]*' -o)
  ramAvail=$(cat /proc/meminfo | grep 'MemAvailable:\ *[0-9]*' | grep '[0-9]*' -o)
  ramUsed=$(($ramAvail - $ramFree))
  echo "Power: $power"
  echo "RAM: $ramUsed"
  printf "%s, %s\n" "$i" "$power" >> "$1-power"
  printf "%s, %s\n" "$i" "$ramUsed" >> "$1-ram"
  sleep 1
done
```

#### Data format
The output data files were of the format:
```
<time>, <ram_used>
<time2>, <ram_used2>
```

#### Plotting script
The (below-par) script used for plotting the plots is here.
It takes as argument the 2 files (like: chrome-ram and firefox-ram).

``` bash
#!/bin/bash

if [ $# -eq 0 ]; then
    echo "No file to be plotted"
    exit 1
fi

metric=$(echo $1 | grep '\-.*' -o)
metric=${metric:1}
echo $metric

# Have to pipe to gnuplot via cat otherwise it does not pause
(cat << EOF
set title "Plot of $metric in Chrome 51 vs Firefox 48"
set term png
set output "$metric-plot.png"
set ylabel "$metric"
set xlabel "Time (seconds)"
set autoscale

set style data histogram
set style histogram gap 1
set style fill solid border -1
set boxwidth 1

set datafile separator ","
set arrow from 25, graph 0 to 25, graph 1 nohead
set arrow from 45, graph 0 to 45, graph 1 nohead
set arrow from 65, graph 0 to 65, graph 1 nohead
set arrow from 85, graph 0 to 85, graph 1 nohead
set arrow from 105, graph 0 to 105, graph 1 nohead
plot '$1' using 2, '$2' using 2
pause -1 "Press Ctrl+D to exit"
EOF
cat /dev/tty) | gnuplot
```
