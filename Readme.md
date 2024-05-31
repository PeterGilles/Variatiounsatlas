# Varatiounsatlas

## Introduction

lorem ipsum asw

## Docker

You can run the app using Docker. First, build the image:

```
docker build -t va .
```

then, run a container from the image:

```
docker run --rm -p 3838:3838 --name va_container va
```

You can now find the app by browsing to `http://localhost:3838/`.
