FROM debian:stable-slim

# copy the built executable
ADD ./reactb /reactb

EXPOSE 8080

# run the executable
CMD ["/reactb"]
