FROM alpine

RUN apk --update add ncurses && rm -rf /var/cache/apk/*

COPY _artifacts /app
CMD [ "/app/sserver/bin/sserver" ]