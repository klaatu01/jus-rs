# JUS (JSON Understated Schema)

JUS is a simplified version of JSON Schema. It is designed to be more human-readable and easier to understand than JSON Schema. It is also designed to be more concise and less verbose than JSON Schema.

## Features

- [x] Basic primitive types `string`, `number`, `boolean`, `array`, and `object`.
- [x] `optional` fields through the `?` operator.
- [x] `nullable` types through the `!` operator.
- [x] `enum` types through the `|` operator.
- [x] `constants` by defining a field type with a string `"value"`, boolean `true` or `false`, or number `0`.
- [x] Type Aliases through `type` keyword.
- [ ] Type property validation using attribute tags `#()` eg. `#(min = 0, max = 10)`.

## JUS vs JSON Schema

JUS:

```jus
type EmailAddress string;

type PhoneNumber string;

type SocialMediaHandle string;

type UserID string;

type ContactInfo {
  email: EmailAddress;
  phone?: PhoneNumber;
  socialMedia?: {
    twitter?: SocialMediaHandle;
    linkedin?: SocialMediaHandle;
    github?: SocialMediaHandle;
  };
};

schema UserProfile {
  id: UserID;
  name: string;
  username: string;
  contactInfo: ContactInfo;
  friends?: [UserID];
  status?: "active" | "inactive" | "banned";
  bio?: !string;
  preferences?: {
    theme: "light" | "dark" | "system";
    notifications: {
      email: boolean;
      sms: boolean;
      push: boolean;
    };
  };
}
```

JSONSchema equivalent:

```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "$id": "https://example.com/schemas/userprofile.json",
  "title": "UserProfile",
  "type": "object",
  "properties": {
    "id": {
      "$ref": "#/$defs/UserID"
    },
    "name": {
      "type": "string"
    },
    "username": {
      "type": "string"
    },
    "contactInfo": {
      "$ref": "#/$defs/ContactInfo"
    },
    "friends": {
      "type": "array",
      "items": {
        "$ref": "#/$defs/UserID"
      }
    },
    "status": {
      "type": "string",
      "enum": ["active", "inactive", "banned"]
    },
    "bio": {
      "anyOf": [
        {
          "type": "string"
        },
        {
          "type": "null"
        }
      ]
    },
    "preferences": {
      "type": "object",
      "properties": {
        "theme": {
          "type": "string",
          "enum": ["light", "dark", "system"]
        },
        "notifications": {
          "type": "object",
          "properties": {
            "email": {
              "type": "boolean"
            },
            "sms": {
              "type": "boolean"
            },
            "push": {
              "type": "boolean"
            }
          },
          "required": ["email", "sms", "push"],
          "additionalProperties": false
        }
      },
      "required": ["theme", "notifications"],
      "additionalProperties": false
    }
  },
  "required": ["id", "name", "username", "contactInfo"],
  "additionalProperties": false,
  "$defs": {
    "EmailAddress": {
      "type": "string"
    },
    "PhoneNumber": {
      "type": "string"
    },
    "SocialMediaHandle": {
      "type": "string"
    },
    "UserID": {
      "type": "string"
    },
    "ContactInfo": {
      "type": "object",
      "properties": {
        "email": {
          "$ref": "#/$defs/EmailAddress"
        },
        "phone": {
          "$ref": "#/$defs/PhoneNumber"
        },
        "socialMedia": {
          "type": "object",
          "properties": {
            "twitter": {
              "$ref": "#/$defs/SocialMediaHandle"
            },
            "linkedin": {
              "$ref": "#/$defs/SocialMediaHandle"
            },
            "github": {
              "$ref": "#/$defs/SocialMediaHandle"
            }
          },
          "additionalProperties": false
        }
      },
      "required": ["email"],
      "additionalProperties": false
    }
  }
}
```
