# What is This?

A minimalist lisp language and computing environment that runs in the browser.

For more information see https://midnightlisp.com

# Development model

No PRs without asking first please, I want to make sure I understand everything in the codebase.

# Setup

```
npm install
npm run postinstall
```

# Run

In separate terminals:

```
npm run esbuild-dev-watch
```

```
npm run generate-purs-watch
```

Port 8000:
```
npm run serve
```

Port 3000:
```
./proxy
```

Go to http://127.0.0.1:3000/

# Test

```
npm run test-watch
```
