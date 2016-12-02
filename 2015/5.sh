if echo -n "$1" | egrep '(.*[aeiou].*){3}' | python 5.py | egrep -v '(ab)|(cd)|(pq)|(xy)' > /dev/null; then
    echo $1
fi
