#!/bin/sh

cat ~/.recoll/idxstatus.txt > /tmp/recoll.out
/usr/bin/recollindex >> /tmp/recoll.out 2>&1
cat ~/.recoll/idxstatus.txt >> /tmp/recoll.out
cat /tmp/recoll.out | mail -s recollindex danielepietroarosio@gmail.com
