for image in *.pdf
do
	echo "${image%.pdf}.tiff"
	gs -dBATCH -dNOPAUSE -sDEVICE=tiff24nc -r300 -dCOLORSCREEN=false -dNOINTERPOLATE -dNOTRANSPARENCY -dUseFastColor=true -sOutputFile="${image%.pdf}.tiff" $image
done