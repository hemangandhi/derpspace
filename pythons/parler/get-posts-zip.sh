DIR="${1:-/tmp}"

pushd
cd "${DIR}"
mkdir parlons
cd parlons

wget https://parlertrick.s3-us-west-2.amazonaws.com/data_no_video.tar.gz
tar -xf data_no_video.tar.gz

popd
