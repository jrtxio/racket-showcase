# Shorty Project Overview

## Project Description
Shorty is a URL shortening application built using Racket as the primary backend language, with a modern web interface. It leverages the Koyo web framework (built on top of Racket's web-server) to provide a full-featured web application with user management, authentication, and URL shortening capabilities.

## Technologies Stack
- **Backend**: Racket programming language with Koyo web framework
- **Frontend**: TypeScript, Sass CSS, with Unpoly JavaScript library for dynamic interactions
- **Database**: PostgreSQL with migrations managed by the North library
- **Build Tools**: Node.js with esbuild for asset compilation and bundling
- **Authentication**: Argon2id hashing algorithm for password security
- **Session Management**: PostgreSQL-backed sessions
- **Mail Service**: Postmark integration for email delivery
- **Process Management**: Chief for managing multiple processes (web server and asset watcher)

## Architecture
- **Component Architecture**: The application uses a component-based architecture managed by the `component` library
- **Configuration**: Environment-based configuration system with default values
- **Database Migrations**: SQL-based migrations in the `migrations/` directory
- **Frontend Build**: TypeScript and Sass files compiled to static assets

## Project Structure
```
shorty/
├── migrations/                 # Database migration files (SQL)
├── node_modules/              # Node.js dependencies
├── resources/                 # Frontend source files
│   ├── css/                   # Sass stylesheets
│   ├── img/                   # Image assets
│   ├── js/                    # TypeScript source files
│   └── locales/               # Localization files
├── shorty/                    # Main Racket application source
│   ├── components/            # Reusable components
│   ├── pages/                 # Page controllers (like dashboard.rkt)
│   ├── config.rkt             # Configuration system
│   ├── dynamic.rkt            # Main application system definition
│   └── console.rkt            # Console/REPL functionality
├── shorty-tests/              # Test suite
├── static/                    # Compiled frontend assets (generated)
├── .env.default               # Environment variable template
├── build.mjs                  # Frontend build script (esbuild)
├── package.json              # Node.js dependencies and scripts
├── Procfile                  # Process definitions for Chief
├── README.md                 # Project documentation
└── tsconfig.json             # TypeScript configuration
```

## Building and Running

### Prerequisites
- Racket programming language
- Node.js version 20 or higher
- PostgreSQL database server with `shorty` and `shorty_tests` databases

### Setup Commands
```bash
# Install Node.js dependencies and build assets
npm install && npm run build

# Install Racket packages
raco pkg install chief
raco pkg install shorty/        # Install main application
raco pkg install shorty-tests/  # Install test suite
```

### Running the Application
```bash
# Copy environment template and start
cp .env.default .env
raco chief start                # Starts both web server and asset watcher
```

### Alternative Commands
```bash
# Run the console/repl
racket shorty/dynamic.rkt console

# Build assets only
npm run build

# Watch and rebuild assets during development
npm run watch
```

## Key Components
- **config.rkt**: Manages application configuration from environment variables
- **dynamic.rkt**: Defines the main system with components (database, sessions, users, etc.)
- **components/**: Various system components like auth, mail, user management
- **pages/**: Web page controllers (e.g., dashboard.rkt)
- **console.rkt**: Provides an interactive REPL for debugging and administration

## Database Schema
- **users**: User accounts with username, password hash, verification status
- **password_reset_requests**: Password reset tokens and tracking

## Development Conventions
- Racket code follows functional programming patterns with component architecture
- Frontend uses TypeScript and Sass with Unpoly for progressive enhancement
- Database migrations use North library with SQL files
- Configuration via environment variables with sensible defaults
- Authentication uses Argon2id for secure password hashing
- Sessions are stored in PostgreSQL for persistence

## Testing
- Tests are located in the `shorty-tests/` directory
- Uses Racket's testing framework with component-based testing patterns
- Requires a separate test database (`shorty_tests`)

## Special Features
- SSL support (can be disabled during development with SHORTY_DEBUG)
- Session management with configurable shelf life
- Email support via Postmark
- Localization support with locale reloading capability
- Job queue system for background processing
- Memory threshold management for continuation handling