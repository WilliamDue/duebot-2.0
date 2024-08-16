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

    if 'bro' in message.content.lower():
        await message.channel.send('Bro, bro, are you serious bro?')

client.run(open('.key', 'r').read())
