# This example requires the 'message_content' intent.

import discord

intents = discord.Intents.default()
intents.message_content = True

client = discord.Client(intents=intents)

"""
@client.event
async def on_ready():
    print(f'We have logged in as {client.user}')
"""

@client.event
async def on_message(message):
    if message.author == client.user:
        return

    if 'true' in message.content.lower():
        await message.channel.send('This is true.')

client.run(open('.key', 'r').read())
