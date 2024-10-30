dev:
	APP_URL="http://localhost:8080" pnpm dev & \
	ENV="development" ghcid -c "stack ghci" --test ":main" & \
	ENV="development" stack ghci --ghci-options "-e Main.development" & \
	wait
