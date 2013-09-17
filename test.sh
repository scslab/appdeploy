echo "launch"
echo "memcached"
echo "PORT=11211"
echo ""

size=`wc -c $1 | cut -d " " -f 1`

echo $size
cat $1

