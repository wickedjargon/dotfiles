# import modules
from tkinter import *
from khalsahp10module import *


# user defined function
def get_dis():
    try:
        filename = str(e1.get())
        with open(filename, 'r') as f:
            locations_from_file = f.readlines()
        # remove `\n` from each list
        temp = []
        for location in locations_from_file:
            temp.append(location.strip('\n'))
        locations = temp
        location_objects = []
        for location in locations:
            location_details = location.split(', ')
            lat = float(location_details[0])
            lon = float(location_details[1])
            desc = location_details[2]
            location_object = GeoPoint(lat, lon, desc)
            location_objects.append(location_object)

        user_point_co_lat = float(e2.get())
        user_point_co_lon = float(e3.get())
        users_point_description = 'TBA'
        user_point = GeoPoint(user_point_co_lat, user_point_co_lon, users_point_description)
        user_point_cord = user_point.GetPoint()
        user_point_lat = user_point_cord[0]
        user_point_lon = user_point_cord[1]
        distance_items = []
        for location_object in location_objects:
            distance_item = {}
            distance_from_user_loc = location_object.Distance([user_point_lat, user_point_lon])
            distance_item['object'] = location_object
            distance_item['distance'] = distance_from_user_loc
            distance_items.append(distance_item)
        # find the closest item in distance_items
        # first initiate closest_item to a random item
        closest_item = distance_item
        # then iterate over distance_items to check if item is closer, redefine closest when True
        for item in distance_items:
            if item['distance'] < closest_item['distance']:
                closest_item = item
        description_of_closest = closest_item['object'].GetDescription()
        co_of_closest = closest_item['object'].GetPoint()
        res = f"{description_of_closest} which is located at {co_of_closest}"
        result.set(res)
    except Exception as err:
        result.set(f"something went wrong: {err}")


tk_object = Tk()
tk_object.configure()
tk_object.title("Find Distance")

# Variable Classes in tkinter
result = StringVar()

# Creating label for each information
# name using widget Label
Label(tk_object, text="Enter the filename: ").grid(row=1, sticky=W)
Label(tk_object, text="Enter your location (lat): ").grid(row=2, sticky=W)
Label(tk_object, text="Enter your location (lon): ").grid(row=3, sticky=W)
Label(tk_object, text="You are closest to :", bg="light grey").grid(row=4, sticky=W)

# Creating label for class variable
# name using widget Entry
Label(tk_object, text="", textvariable=result, bg="light grey").grid(row=4, column=1, sticky=W)

e1 = Entry(tk_object, width=50)  # filename
e1.grid(row=1, column=1)
e2 = Entry(tk_object, width=50)  # location (lat)
e2.grid(row=2, column=1)
e3 = Entry(tk_object, width=50)  # location (lon)
e3.grid(row=3, column=1)

# creating a button using the widget
b = Button(tk_object, text="Get results", command=get_dis, bg="white")
b.grid(
    row=1,
    column=2,
    columnspan=2,
    rowspan=2,
    padx=5,
    pady=5,
)

mainloop()
