library(pins)

# Register Board for data pull
board_register("https://raw.githubusercontent.com/predictcrypto/pins/master/","pins_data")

# Pull data
cryptovoxels_all_parcels = pin_get("cryptovoxels_all_parcels", "pins_data")
cryptovoxels_orders = pin_get("cryptovoxels_all_orders", "pins_data")
decentraland_thegraph = pin_get("decentraland_MANA_orders", "pins_data")

# Write csv
write.csv(cryptovoxels_all_parcels, "cryptovoxels/data/cryptovoxels_all_parcels.csv")
write.csv(cryptovoxels_orders, "cryptovoxels/data/cryptovoxels_orders.csv")
write.csv(decentraland_thegraph, "cryptovoxels/data/decentraland_thegraph.csv")
