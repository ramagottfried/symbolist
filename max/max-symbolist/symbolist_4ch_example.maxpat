{
	"patcher" : 	{
		"fileversion" : 1,
		"appversion" : 		{
			"major" : 7,
			"minor" : 3,
			"revision" : 4,
			"architecture" : "x64",
			"modernui" : 1
		}
,
		"rect" : [ 559.0, 79.0, 1152.0, 787.0 ],
		"bglocked" : 0,
		"openinpresentation" : 0,
		"default_fontsize" : 12.0,
		"default_fontface" : 0,
		"default_fontname" : "Arial",
		"gridonopen" : 1,
		"gridsize" : [ 15.0, 15.0 ],
		"gridsnaponopen" : 1,
		"objectsnaponopen" : 1,
		"statusbarvisible" : 2,
		"toolbarvisible" : 1,
		"lefttoolbarpinned" : 0,
		"toptoolbarpinned" : 0,
		"righttoolbarpinned" : 0,
		"bottomtoolbarpinned" : 0,
		"toolbars_unpinned_last_save" : 0,
		"tallnewobj" : 0,
		"boxanimatetime" : 200,
		"enablehscroll" : 1,
		"enablevscroll" : 1,
		"devicewidth" : 0.0,
		"description" : "",
		"digest" : "",
		"tags" : "",
		"style" : "",
		"subpatcher_template" : "Default Max 7",
		"boxes" : [ 			{
				"box" : 				{
					"id" : "obj-44",
					"maxclass" : "comment",
					"numinlets" : 1,
					"numoutlets" : 0,
					"patching_rect" : [ 456.0, 214.5, 356.0, 20.0 ],
					"presentation_rect" : [ 455.0, 214.5, 0.0, 0.0 ],
					"style" : "",
					"text" : "what if two symbols have the same name in a group?"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-43",
					"linecount" : 6,
					"maxclass" : "comment",
					"numinlets" : 1,
					"numoutlets" : 0,
					"patching_rect" : [ 456.0, 119.5, 356.0, 87.0 ],
					"style" : "",
					"text" : "subsymbols should be renamed by their /name for lookup output\n\nwhen composing and dealing with symbols it makes some sense to treat them as non-heierarchaial atomic units, but on lookup we are mainly interested in using the inter relationships that were specified by names and staff references."
				}

			}
, 			{
				"box" : 				{
					"fontface" : 0,
					"fontsize" : 12.0,
					"id" : "obj-41",
					"linecount" : 74,
					"maxclass" : "o.display",
					"numinlets" : 1,
					"numoutlets" : 1,
					"outlettype" : [ "" ],
					"patching_rect" : [ 94.0, 255.0, 603.0, 1026.0 ],
					"text" : "/time/ratio : 0.16835,\n/trajectory_event/duration/lookup/xy : [0.172817, 0.0263158],\n/trajectory_event/trajectory/lookup/xy : [0.0431991, 0.649481],\n/trajectory_event/name : \"trajectory_event\",\n/trajectory_event/type : \"group\",\n/trajectory_event/id : \"group/palette\",\n/trajectory_event/staff : \"spat_1\",\n/trajectory_event/x : 7.,\n/trajectory_event/y : -123.,\n/trajectory_event/w : 297.,\n/trajectory_event/h : 76.,\n/trajectory_event/color : [0., 0., 0., 1.],\n/trajectory_event/numsymbols : 3,\n/trajectory_event/subsymbol/1/name : \"frame\",\n/trajectory_event/subsymbol/1/type : \"rectangle\",\n/trajectory_event/subsymbol/1/id : \"frame/palette\",\n/trajectory_event/subsymbol/1/staff : \"<none>\",\n/trajectory_event/subsymbol/1/x : 0.,\n/trajectory_event/subsymbol/1/y : 36.5,\n/trajectory_event/subsymbol/1/w : 85.,\n/trajectory_event/subsymbol/1/h : 73.,\n/trajectory_event/subsymbol/1/color : [0., 0., 0., 1.],\n/trajectory_event/subsymbol/1/fill : 0,\n/trajectory_event/subsymbol/1/stroke/thickness : 2.,\n/trajectory_event/subsymbol/1/rotation : 0.,\n/trajectory_event/subsymbol/2/name : \"duration\",\n/trajectory_event/subsymbol/2/type : \"path\",\n/trajectory_event/subsymbol/2/id : \"duration/palette\",\n/trajectory_event/subsymbol/2/staff : \"<none>\",\n/trajectory_event/subsymbol/2/x : 0.,\n/trajectory_event/subsymbol/2/y : 72.,\n/trajectory_event/subsymbol/2/w : 297.,\n/trajectory_event/subsymbol/2/h : 4.,\n/trajectory_event/subsymbol/2/color : [0., 0., 0., 1.],\n/trajectory_event/subsymbol/2/num_sub_paths : 1,\n/trajectory_event/subsymbol/2/path/0/str : \"m 2 2 l 295 2\",\n/trajectory_event/subsymbol/2/path/0/length : 293.,\n/trajectory_event/subsymbol/2/path/0/time/start : 0.02,\n/trajectory_event/subsymbol/2/path/0/time/duration : 2.93,\n/trajectory_event/subsymbol/2/fill : 0,\n/trajectory_event/subsymbol/2/stroke/thickness : 2.,\n/trajectory_event/subsymbol/3/name : \"trajectory\",\n/trajectory_event/subsymbol/3/type : \"path\",\n/trajectory_event/subsymbol/3/id : \"trajectory/palette\",\n/trajectory_event/subsymbol/3/staff : \"<none>\",\n/trajectory_event/subsymbol/3/x : 11.,\n/trajectory_event/subsymbol/3/y : 9.,\n/trajectory_event/subsymbol/3/w : 29.,\n/trajectory_event/subsymbol/3/h : 52.,\n/trajectory_event/subsymbol/3/color : [0., 0., 0., 1.],\n/trajectory_event/subsymbol/3/num_sub_paths : 1,\n/trajectory_event/subsymbol/3/path/0/str : \"m 2 44.71 q 39.5 66.044 20.75 2.043\",\n/trajectory_event/subsymbol/3/path/0/length : 70.2279,\n/trajectory_event/subsymbol/3/path/0/time/start : 0.02,\n/trajectory_event/subsymbol/3/path/0/time/duration : 0.25,\n/trajectory_event/subsymbol/3/fill : 0,\n/trajectory_event/subsymbol/3/stroke/thickness : 2.,\n/trajectory_event/time/start : 0.07,\n/trajectory_event/time/duration : 2.97,\n/trajectory_event/subsymbol/2/num_sub_paths : 1,\n/trajectory_event/subsymbol/2/path/0/str : \"m 2 2 l 295 2\",\n/trajectory_event/subsymbol/2/path/0/length : 293.,\n/trajectory_event/subsymbol/2/path/0/time/start : 0.02,\n/trajectory_event/subsymbol/2/path/0/time/duration : 2.93,\n/trajectory_event/subsymbol/2/fill : 0,\n/trajectory_event/subsymbol/2/stroke/thickness : 2.,\n/trajectory_event/subsymbol/3/num_sub_paths : 1,\n/trajectory_event/subsymbol/3/path/0/str : \"m 2 44.71 q 39.5 66.044 20.75 2.043\",\n/trajectory_event/subsymbol/3/path/0/length : 70.2279,\n/trajectory_event/subsymbol/3/path/0/time/start : 0.02,\n/trajectory_event/subsymbol/3/path/0/time/duration : 0.25,\n/trajectory_event/subsymbol/3/fill : 0,\n/trajectory_event/subsymbol/3/stroke/thickness : 2.,\n/state : 0"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-31",
					"linecount" : 3,
					"maxclass" : "newobj",
					"numinlets" : 1,
					"numoutlets" : 2,
					"outlettype" : [ "", "FullPacket" ],
					"patching_rect" : [ 88.0, 317.0, 203.0, 49.0 ],
					"style" : "",
					"text" : "o.gather.select /trajectory_event/trajectory /time/ratio"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-17",
					"maxclass" : "newobj",
					"numinlets" : 1,
					"numoutlets" : 2,
					"outlettype" : [ "", "FullPacket" ],
					"patching_rect" : [ 88.0, 164.0, 61.0, 22.0 ],
					"style" : "",
					"text" : "o.route /0"
				}

			}
, 			{
				"box" : 				{
					"fontface" : 0,
					"fontsize" : 12.0,
					"id" : "obj-8",
					"linecount" : 3,
					"maxclass" : "o.expr.codebox",
					"numinlets" : 1,
					"numoutlets" : 2,
					"outlettype" : [ "FullPacket", "FullPacket" ],
					"patching_rect" : [ 472.0, 49.5, 358.0, 59.0 ],
					"text" : "/traj = /trajectory_event/trajectory/lookup/xy,\n\n/spat/source = [ \"source\", 1, \"xy\", /traj]"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-13",
					"maxclass" : "message",
					"numinlets" : 2,
					"numoutlets" : 1,
					"outlettype" : [ "" ],
					"patching_rect" : [ 376.0, 18.0, 50.0, 22.0 ],
					"style" : "",
					"text" : "clear"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-15",
					"maxclass" : "message",
					"numinlets" : 2,
					"numoutlets" : 1,
					"outlettype" : [ "" ],
					"patching_rect" : [ 399.0, 42.0, 50.0, 22.0 ],
					"style" : "",
					"text" : "dump"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-38",
					"maxclass" : "button",
					"numinlets" : 1,
					"numoutlets" : 1,
					"outlettype" : [ "bang" ],
					"patching_rect" : [ 366.0, 138.5, 24.0, 24.0 ],
					"style" : ""
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-18",
					"maxclass" : "newobj",
					"numinlets" : 1,
					"numoutlets" : 1,
					"outlettype" : [ "FullPacket" ],
					"patcher" : 					{
						"fileversion" : 1,
						"appversion" : 						{
							"major" : 7,
							"minor" : 3,
							"revision" : 4,
							"architecture" : "x64",
							"modernui" : 1
						}
,
						"rect" : [ 63.0, 104.0, 640.0, 480.0 ],
						"bglocked" : 0,
						"openinpresentation" : 0,
						"default_fontsize" : 12.0,
						"default_fontface" : 0,
						"default_fontname" : "Arial",
						"gridonopen" : 1,
						"gridsize" : [ 15.0, 15.0 ],
						"gridsnaponopen" : 1,
						"objectsnaponopen" : 1,
						"statusbarvisible" : 2,
						"toolbarvisible" : 1,
						"lefttoolbarpinned" : 0,
						"toptoolbarpinned" : 0,
						"righttoolbarpinned" : 0,
						"bottomtoolbarpinned" : 0,
						"toolbars_unpinned_last_save" : 0,
						"tallnewobj" : 0,
						"boxanimatetime" : 200,
						"enablehscroll" : 1,
						"enablevscroll" : 1,
						"devicewidth" : 0.0,
						"description" : "",
						"digest" : "",
						"tags" : "",
						"style" : "",
						"subpatcher_template" : "Default Max 7",
						"boxes" : [ 							{
								"box" : 								{
									"id" : "obj-32",
									"maxclass" : "newobj",
									"numinlets" : 1,
									"numoutlets" : 1,
									"outlettype" : [ "FullPacket" ],
									"patching_rect" : [ 50.0, 198.0, 53.0, 22.0 ],
									"style" : "",
									"text" : "o.flatten"
								}

							}
, 							{
								"box" : 								{
									"id" : "obj-28",
									"maxclass" : "newobj",
									"numinlets" : 1,
									"numoutlets" : 2,
									"outlettype" : [ "", "FullPacket" ],
									"patching_rect" : [ 50.0, 129.0, 93.0, 22.0 ],
									"style" : "",
									"text" : "o.route /symbol"
								}

							}
, 							{
								"box" : 								{
									"id" : "obj-25",
									"maxclass" : "newobj",
									"numinlets" : 2,
									"numoutlets" : 1,
									"outlettype" : [ "FullPacket" ],
									"patching_rect" : [ 50.0, 235.0, 71.0, 22.0 ],
									"style" : "",
									"text" : "o.downcast"
								}

							}
, 							{
								"box" : 								{
									"id" : "obj-27",
									"maxclass" : "newobj",
									"numinlets" : 1,
									"numoutlets" : 2,
									"outlettype" : [ "", "FullPacket" ],
									"patching_rect" : [ 50.0, 165.0, 59.0, 22.0 ],
									"style" : "",
									"text" : "o.route /*"
								}

							}
, 							{
								"box" : 								{
									"id" : "obj-20",
									"maxclass" : "newobj",
									"numinlets" : 1,
									"numoutlets" : 1,
									"outlettype" : [ "FullPacket" ],
									"patching_rect" : [ 50.0, 100.0, 63.0, 22.0 ],
									"style" : "",
									"text" : "o.explode"
								}

							}
, 							{
								"box" : 								{
									"comment" : "",
									"id" : "obj-35",
									"index" : 1,
									"maxclass" : "inlet",
									"numinlets" : 0,
									"numoutlets" : 1,
									"outlettype" : [ "" ],
									"patching_rect" : [ 50.0, 40.0, 30.0, 30.0 ],
									"style" : ""
								}

							}
, 							{
								"box" : 								{
									"comment" : "",
									"id" : "obj-36",
									"index" : 1,
									"maxclass" : "outlet",
									"numinlets" : 1,
									"numoutlets" : 0,
									"patching_rect" : [ 50.0, 317.0, 30.0, 30.0 ],
									"style" : ""
								}

							}
 ],
						"lines" : [ 							{
								"patchline" : 								{
									"destination" : [ "obj-28", 0 ],
									"source" : [ "obj-20", 0 ]
								}

							}
, 							{
								"patchline" : 								{
									"destination" : [ "obj-36", 0 ],
									"source" : [ "obj-25", 0 ]
								}

							}
, 							{
								"patchline" : 								{
									"destination" : [ "obj-32", 0 ],
									"source" : [ "obj-27", 0 ]
								}

							}
, 							{
								"patchline" : 								{
									"destination" : [ "obj-27", 0 ],
									"source" : [ "obj-28", 0 ]
								}

							}
, 							{
								"patchline" : 								{
									"destination" : [ "obj-25", 0 ],
									"source" : [ "obj-32", 0 ]
								}

							}
, 							{
								"patchline" : 								{
									"destination" : [ "obj-20", 0 ],
									"source" : [ "obj-35", 0 ]
								}

							}
 ]
					}
,
					"patching_rect" : [ 313.0, 210.5, 37.0, 22.0 ],
					"saved_object_attributes" : 					{
						"description" : "",
						"digest" : "",
						"globalpatchername" : "",
						"style" : "",
						"tags" : ""
					}
,
					"style" : "",
					"text" : "p set"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-19",
					"maxclass" : "button",
					"numinlets" : 1,
					"numoutlets" : 1,
					"outlettype" : [ "bang" ],
					"patching_rect" : [ 313.0, 144.5, 24.0, 24.0 ],
					"style" : ""
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-20",
					"maxclass" : "newobj",
					"numinlets" : 2,
					"numoutlets" : 1,
					"outlettype" : [ "" ],
					"patcher" : 					{
						"fileversion" : 1,
						"appversion" : 						{
							"major" : 7,
							"minor" : 3,
							"revision" : 4,
							"architecture" : "x64",
							"modernui" : 1
						}
,
						"rect" : [ 38.0, 79.0, 1012.0, 681.0 ],
						"bglocked" : 0,
						"openinpresentation" : 0,
						"default_fontsize" : 12.0,
						"default_fontface" : 0,
						"default_fontname" : "Arial",
						"gridonopen" : 1,
						"gridsize" : [ 15.0, 15.0 ],
						"gridsnaponopen" : 1,
						"objectsnaponopen" : 1,
						"statusbarvisible" : 2,
						"toolbarvisible" : 1,
						"lefttoolbarpinned" : 0,
						"toptoolbarpinned" : 0,
						"righttoolbarpinned" : 0,
						"bottomtoolbarpinned" : 0,
						"toolbars_unpinned_last_save" : 0,
						"tallnewobj" : 0,
						"boxanimatetime" : 200,
						"enablehscroll" : 1,
						"enablevscroll" : 1,
						"devicewidth" : 0.0,
						"description" : "",
						"digest" : "",
						"tags" : "",
						"style" : "",
						"subpatcher_template" : "Default Max 7",
						"boxes" : [ 							{
								"box" : 								{
									"comment" : "",
									"id" : "obj-1",
									"index" : 1,
									"maxclass" : "inlet",
									"numinlets" : 0,
									"numoutlets" : 1,
									"outlettype" : [ "bang" ],
									"patching_rect" : [ 25.0, 31.0, 30.0, 30.0 ],
									"style" : ""
								}

							}
, 							{
								"box" : 								{
									"fontface" : 0,
									"fontsize" : 12.0,
									"id" : "obj-18",
									"linecount" : 90,
									"maxclass" : "o.compose",
									"numinlets" : 2,
									"numoutlets" : 1,
									"outlettype" : [ "" ],
									"patching_rect" : [ 50.0, 100.0, 563.0, 1233.0 ],
									"saved_bundle_data" : [ 35, 98, 117, 110, 100, 108, 101, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 28, 47, 115, 121, 109, 98, 111, 108, 47, 48, 47, 110, 97, 109, 101, 0, 0, 44, 115, 0, 0, 115, 112, 97, 116, 0, 0, 0, 0, 0, 0, 0, 28, 47, 115, 121, 109, 98, 111, 108, 47, 48, 47, 116, 121, 112, 101, 0, 0, 44, 115, 0, 0, 115, 116, 97, 102, 102, 0, 0, 0, 0, 0, 0, 28, 47, 115, 121, 109, 98, 111, 108, 47, 48, 47, 105, 100, 0, 0, 0, 0, 44, 115, 0, 0, 115, 112, 97, 116, 95, 49, 0, 0, 0, 0, 0, 28, 47, 115, 121, 109, 98, 111, 108, 47, 48, 47, 115, 116, 97, 102, 102, 0, 44, 115, 0, 0, 60, 110, 111, 110, 101, 62, 0, 0, 0, 0, 0, 24, 47, 115, 121, 109, 98, 111, 108, 47, 48, 47, 120, 0, 44, 100, 0, 0, 64, 84, -64, 0, 0, 0, 0, 0, 0, 0, 0, 24, 47, 115, 121, 109, 98, 111, 108, 47, 48, 47, 121, 0, 44, 100, 0, 0, 64, 109, 64, 0, 0, 0, 0, 0, 0, 0, 0, 24, 47, 115, 121, 109, 98, 111, 108, 47, 48, 47, 119, 0, 44, 100, 0, 0, 64, 125, -80, 0, 0, 0, 0, 0, 0, 0, 0, 24, 47, 115, 121, 109, 98, 111, 108, 47, 48, 47, 104, 0, 44, 100, 0, 0, 64, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 56, 47, 115, 121, 109, 98, 111, 108, 47, 48, 47, 99, 111, 108, 111, 114, 0, 44, 100, 100, 100, 100, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 63, -16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 40, 47, 115, 121, 109, 98, 111, 108, 47, 48, 47, 115, 116, 97, 102, 102, 83, 121, 109, 98, 111, 108, 47, 110, 97, 109, 101, 0, 0, 44, 115, 0, 0, 112, 97, 116, 104, 0, 0, 0, 0, 0, 0, 0, 40, 47, 115, 121, 109, 98, 111, 108, 47, 48, 47, 115, 116, 97, 102, 102, 83, 121, 109, 98, 111, 108, 47, 116, 121, 112, 101, 0, 0, 44, 115, 0, 0, 112, 97, 116, 104, 0, 0, 0, 0, 0, 0, 0, 48, 47, 115, 121, 109, 98, 111, 108, 47, 48, 47, 115, 116, 97, 102, 102, 83, 121, 109, 98, 111, 108, 47, 105, 100, 0, 0, 0, 0, 44, 115, 0, 0, 112, 97, 116, 104, 95, 112, 97, 108, 101, 116, 116, 101, 0, 0, 0, 0, 0, 0, 0, 40, 47, 115, 121, 109, 98, 111, 108, 47, 48, 47, 115, 116, 97, 102, 102, 83, 121, 109, 98, 111, 108, 47, 115, 116, 97, 102, 102, 0, 44, 115, 0, 0, 60, 110, 111, 110, 101, 62, 0, 0, 0, 0, 0, 36, 47, 115, 121, 109, 98, 111, 108, 47, 48, 47, 115, 116, 97, 102, 102, 83, 121, 109, 98, 111, 108, 47, 120, 0, 44, 100, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 36, 47, 115, 121, 109, 98, 111, 108, 47, 48, 47, 115, 116, 97, 102, 102, 83, 121, 109, 98, 111, 108, 47, 121, 0, 44, 100, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 36, 47, 115, 121, 109, 98, 111, 108, 47, 48, 47, 115, 116, 97, 102, 102, 83, 121, 109, 98, 111, 108, 47, 119, 0, 44, 100, 0, 0, 64, 125, -80, 0, 0, 0, 0, 0, 0, 0, 0, 36, 47, 115, 121, 109, 98, 111, 108, 47, 48, 47, 115, 116, 97, 102, 102, 83, 121, 109, 98, 111, 108, 47, 104, 0, 44, 100, 0, 0, 64, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 68, 47, 115, 121, 109, 98, 111, 108, 47, 48, 47, 115, 116, 97, 102, 102, 83, 121, 109, 98, 111, 108, 47, 99, 111, 108, 111, 114, 0, 44, 100, 100, 100, 100, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 63, -16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 44, 47, 115, 121, 109, 98, 111, 108, 47, 48, 47, 115, 116, 97, 102, 102, 83, 121, 109, 98, 111, 108, 47, 110, 117, 109, 95, 115, 117, 98, 95, 112, 97, 116, 104, 115, 0, 44, 105, 0, 0, 0, 0, 0, 1, 0, 0, 0, 56, 47, 115, 121, 109, 98, 111, 108, 47, 48, 47, 115, 116, 97, 102, 102, 83, 121, 109, 98, 111, 108, 47, 112, 97, 116, 104, 47, 48, 47, 115, 116, 114, 0, 0, 0, 0, 44, 115, 0, 0, 109, 32, 50, 32, 50, 32, 108, 32, 52, 55, 51, 32, 50, 0, 0, 0, 0, 0, 0, 48, 47, 115, 121, 109, 98, 111, 108, 47, 48, 47, 115, 116, 97, 102, 102, 83, 121, 109, 98, 111, 108, 47, 112, 97, 116, 104, 47, 48, 47, 108, 101, 110, 103, 116, 104, 0, 44, 100, 0, 0, 64, 125, 112, 0, 0, 0, 0, 0, 0, 0, 0, 52, 47, 115, 121, 109, 98, 111, 108, 47, 48, 47, 115, 116, 97, 102, 102, 83, 121, 109, 98, 111, 108, 47, 112, 97, 116, 104, 47, 48, 47, 116, 105, 109, 101, 47, 115, 116, 97, 114, 116, 0, 44, 100, 0, 0, 63, -108, 122, -31, 71, -82, 20, 123, 0, 0, 0, 56, 47, 115, 121, 109, 98, 111, 108, 47, 48, 47, 115, 116, 97, 102, 102, 83, 121, 109, 98, 111, 108, 47, 112, 97, 116, 104, 47, 48, 47, 116, 105, 109, 101, 47, 100, 117, 114, 97, 116, 105, 111, 110, 0, 0, 44, 100, 0, 0, 64, 18, -41, 10, 61, 112, -93, -41, 0, 0, 0, 36, 47, 115, 121, 109, 98, 111, 108, 47, 48, 47, 115, 116, 97, 102, 102, 83, 121, 109, 98, 111, 108, 47, 102, 105, 108, 108, 0, 0, 44, 105, 0, 0, 0, 0, 0, 0, 0, 0, 0, 52, 47, 115, 121, 109, 98, 111, 108, 47, 48, 47, 115, 116, 97, 102, 102, 83, 121, 109, 98, 111, 108, 47, 115, 116, 114, 111, 107, 101, 47, 116, 104, 105, 99, 107, 110, 101, 115, 115, 0, 0, 44, 100, 0, 0, 64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 36, 47, 115, 121, 109, 98, 111, 108, 47, 48, 47, 116, 105, 109, 101, 47, 115, 116, 97, 114, 116, 0, 0, 0, 0, 44, 100, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 36, 47, 115, 121, 109, 98, 111, 108, 47, 48, 47, 116, 105, 109, 101, 47, 100, 117, 114, 97, 116, 105, 111, 110, 0, 44, 100, 0, 0, 64, 19, 0, 0, 0, 0, 0, 0, 0, 0, 0, 44, 47, 115, 121, 109, 98, 111, 108, 47, 48, 47, 115, 116, 97, 102, 102, 83, 121, 109, 98, 111, 108, 47, 110, 117, 109, 95, 115, 117, 98, 95, 112, 97, 116, 104, 115, 0, 44, 105, 0, 0, 0, 0, 0, 1, 0, 0, 0, 56, 47, 115, 121, 109, 98, 111, 108, 47, 48, 47, 115, 116, 97, 102, 102, 83, 121, 109, 98, 111, 108, 47, 112, 97, 116, 104, 47, 48, 47, 115, 116, 114, 0, 0, 0, 0, 44, 115, 0, 0, 109, 32, 50, 32, 50, 32, 108, 32, 52, 55, 51, 32, 50, 0, 0, 0, 0, 0, 0, 48, 47, 115, 121, 109, 98, 111, 108, 47, 48, 47, 115, 116, 97, 102, 102, 83, 121, 109, 98, 111, 108, 47, 112, 97, 116, 104, 47, 48, 47, 108, 101, 110, 103, 116, 104, 0, 44, 100, 0, 0, 64, 125, 112, 0, 0, 0, 0, 0, 0, 0, 0, 52, 47, 115, 121, 109, 98, 111, 108, 47, 48, 47, 115, 116, 97, 102, 102, 83, 121, 109, 98, 111, 108, 47, 112, 97, 116, 104, 47, 48, 47, 116, 105, 109, 101, 47, 115, 116, 97, 114, 116, 0, 44, 100, 0, 0, 63, -108, 122, -31, 71, -82, 20, 123, 0, 0, 0, 56, 47, 115, 121, 109, 98, 111, 108, 47, 48, 47, 115, 116, 97, 102, 102, 83, 121, 109, 98, 111, 108, 47, 112, 97, 116, 104, 47, 48, 47, 116, 105, 109, 101, 47, 100, 117, 114, 97, 116, 105, 111, 110, 0, 0, 44, 100, 0, 0, 64, 18, -41, 10, 61, 112, -93, -41, 0, 0, 0, 36, 47, 115, 121, 109, 98, 111, 108, 47, 48, 47, 115, 116, 97, 102, 102, 83, 121, 109, 98, 111, 108, 47, 102, 105, 108, 108, 0, 0, 44, 105, 0, 0, 0, 0, 0, 0, 0, 0, 0, 52, 47, 115, 121, 109, 98, 111, 108, 47, 48, 47, 115, 116, 97, 102, 102, 83, 121, 109, 98, 111, 108, 47, 115, 116, 114, 111, 107, 101, 47, 116, 104, 105, 99, 107, 110, 101, 115, 115, 0, 0, 44, 100, 0, 0, 64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 40, 47, 115, 121, 109, 98, 111, 108, 47, 49, 47, 110, 97, 109, 101, 0, 0, 44, 115, 0, 0, 116, 114, 97, 106, 101, 99, 116, 111, 114, 121, 95, 101, 118, 101, 110, 116, 0, 0, 0, 0, 0, 0, 0, 28, 47, 115, 121, 109, 98, 111, 108, 47, 49, 47, 116, 121, 112, 101, 0, 0, 44, 115, 0, 0, 103, 114, 111, 117, 112, 0, 0, 0, 0, 0, 0, 36, 47, 115, 121, 109, 98, 111, 108, 47, 49, 47, 105, 100, 0, 0, 0, 0, 44, 115, 0, 0, 103, 114, 111, 117, 112, 47, 112, 97, 108, 101, 116, 116, 101, 0, 0, 0, 0, 0, 0, 28, 47, 115, 121, 109, 98, 111, 108, 47, 49, 47, 115, 116, 97, 102, 102, 0, 44, 115, 0, 0, 115, 112, 97, 116, 95, 49, 0, 0, 0, 0, 0, 24, 47, 115, 121, 109, 98, 111, 108, 47, 49, 47, 120, 0, 44, 100, 0, 0, 64, 86, -128, 0, 0, 0, 0, 0, 0, 0, 0, 24, 47, 115, 121, 109, 98, 111, 108, 47, 49, 47, 121, 0, 44, 100, 0, 0, 64, 91, -64, 0, 0, 0, 0, 0, 0, 0, 0, 24, 47, 115, 121, 109, 98, 111, 108, 47, 49, 47, 119, 0, 44, 100, 0, 0, 64, 114, -112, 0, 0, 0, 0, 0, 0, 0, 0, 24, 47, 115, 121, 109, 98, 111, 108, 47, 49, 47, 104, 0, 44, 100, 0, 0, 64, 83, 0, 0, 0, 0, 0, 0, 0, 0, 0, 56, 47, 115, 121, 109, 98, 111, 108, 47, 49, 47, 99, 111, 108, 111, 114, 0, 44, 100, 100, 100, 100, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 63, -16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 32, 47, 115, 121, 109, 98, 111, 108, 47, 49, 47, 110, 117, 109, 115, 121, 109, 98, 111, 108, 115, 0, 0, 0, 0, 44, 105, 0, 0, 0, 0, 0, 3, 0, 0, 0, 40, 47, 115, 121, 109, 98, 111, 108, 47, 49, 47, 115, 117, 98, 115, 121, 109, 98, 111, 108, 47, 49, 47, 110, 97, 109, 101, 0, 0, 44, 115, 0, 0, 102, 114, 97, 109, 101, 0, 0, 0, 0, 0, 0, 44, 47, 115, 121, 109, 98, 111, 108, 47, 49, 47, 115, 117, 98, 115, 121, 109, 98, 111, 108, 47, 49, 47, 116, 121, 112, 101, 0, 0, 44, 115, 0, 0, 114, 101, 99, 116, 97, 110, 103, 108, 101, 0, 0, 0, 0, 0, 0, 48, 47, 115, 121, 109, 98, 111, 108, 47, 49, 47, 115, 117, 98, 115, 121, 109, 98, 111, 108, 47, 49, 47, 105, 100, 0, 0, 0, 0, 44, 115, 0, 0, 102, 114, 97, 109, 101, 47, 112, 97, 108, 101, 116, 116, 101, 0, 0, 0, 0, 0, 0, 40, 47, 115, 121, 109, 98, 111, 108, 47, 49, 47, 115, 117, 98, 115, 121, 109, 98, 111, 108, 47, 49, 47, 115, 116, 97, 102, 102, 0, 44, 115, 0, 0, 60, 110, 111, 110, 101, 62, 0, 0, 0, 0, 0, 36, 47, 115, 121, 109, 98, 111, 108, 47, 49, 47, 115, 117, 98, 115, 121, 109, 98, 111, 108, 47, 49, 47, 120, 0, 44, 100, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 36, 47, 115, 121, 109, 98, 111, 108, 47, 49, 47, 115, 117, 98, 115, 121, 109, 98, 111, 108, 47, 49, 47, 121, 0, 44, 100, 0, 0, 64, 66, 64, 0, 0, 0, 0, 0, 0, 0, 0, 36, 47, 115, 121, 109, 98, 111, 108, 47, 49, 47, 115, 117, 98, 115, 121, 109, 98, 111, 108, 47, 49, 47, 119, 0, 44, 100, 0, 0, 64, 85, 64, 0, 0, 0, 0, 0, 0, 0, 0, 36, 47, 115, 121, 109, 98, 111, 108, 47, 49, 47, 115, 117, 98, 115, 121, 109, 98, 111, 108, 47, 49, 47, 104, 0, 44, 100, 0, 0, 64, 82, 64, 0, 0, 0, 0, 0, 0, 0, 0, 68, 47, 115, 121, 109, 98, 111, 108, 47, 49, 47, 115, 117, 98, 115, 121, 109, 98, 111, 108, 47, 49, 47, 99, 111, 108, 111, 114, 0, 44, 100, 100, 100, 100, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 63, -16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 36, 47, 115, 121, 109, 98, 111, 108, 47, 49, 47, 115, 117, 98, 115, 121, 109, 98, 111, 108, 47, 49, 47, 102, 105, 108, 108, 0, 0, 44, 105, 0, 0, 0, 0, 0, 0, 0, 0, 0, 52, 47, 115, 121, 109, 98, 111, 108, 47, 49, 47, 115, 117, 98, 115, 121, 109, 98, 111, 108, 47, 49, 47, 115, 116, 114, 111, 107, 101, 47, 116, 104, 105, 99, 107, 110, 101, 115, 115, 0, 0, 44, 100, 0, 0, 64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 44, 47, 115, 121, 109, 98, 111, 108, 47, 49, 47, 115, 117, 98, 115, 121, 109, 98, 111, 108, 47, 49, 47, 114, 111, 116, 97, 116, 105, 111, 110, 0, 0, 44, 100, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 44, 47, 115, 121, 109, 98, 111, 108, 47, 49, 47, 115, 117, 98, 115, 121, 109, 98, 111, 108, 47, 50, 47, 110, 97, 109, 101, 0, 0, 44, 115, 0, 0, 100, 117, 114, 97, 116, 105, 111, 110, 0, 0, 0, 0, 0, 0, 0, 40, 47, 115, 121, 109, 98, 111, 108, 47, 49, 47, 115, 117, 98, 115, 121, 109, 98, 111, 108, 47, 50, 47, 116, 121, 112, 101, 0, 0, 44, 115, 0, 0, 112, 97, 116, 104, 0, 0, 0, 0, 0, 0, 0, 52, 47, 115, 121, 109, 98, 111, 108, 47, 49, 47, 115, 117, 98, 115, 121, 109, 98, 111, 108, 47, 50, 47, 105, 100, 0, 0, 0, 0, 44, 115, 0, 0, 100, 117, 114, 97, 116, 105, 111, 110, 47, 112, 97, 108, 101, 116, 116, 101, 0, 0, 0, 0, 0, 0, 0, 40, 47, 115, 121, 109, 98, 111, 108, 47, 49, 47, 115, 117, 98, 115, 121, 109, 98, 111, 108, 47, 50, 47, 115, 116, 97, 102, 102, 0, 44, 115, 0, 0, 60, 110, 111, 110, 101, 62, 0, 0, 0, 0, 0, 36, 47, 115, 121, 109, 98, 111, 108, 47, 49, 47, 115, 117, 98, 115, 121, 109, 98, 111, 108, 47, 50, 47, 120, 0, 44, 100, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 36, 47, 115, 121, 109, 98, 111, 108, 47, 49, 47, 115, 117, 98, 115, 121, 109, 98, 111, 108, 47, 50, 47, 121, 0, 44, 100, 0, 0, 64, 82, 0, 0, 0, 0, 0, 0, 0, 0, 0, 36, 47, 115, 121, 109, 98, 111, 108, 47, 49, 47, 115, 117, 98, 115, 121, 109, 98, 111, 108, 47, 50, 47, 119, 0, 44, 100, 0, 0, 64, 114, -112, 0, 0, 0, 0, 0, 0, 0, 0, 36, 47, 115, 121, 109, 98, 111, 108, 47, 49, 47, 115, 117, 98, 115, 121, 109, 98, 111, 108, 47, 50, 47, 104, 0, 44, 100, 0, 0, 64, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 68, 47, 115, 121, 109, 98, 111, 108, 47, 49, 47, 115, 117, 98, 115, 121, 109, 98, 111, 108, 47, 50, 47, 99, 111, 108, 111, 114, 0, 44, 100, 100, 100, 100, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 63, -16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 44, 47, 115, 121, 109, 98, 111, 108, 47, 49, 47, 115, 117, 98, 115, 121, 109, 98, 111, 108, 47, 50, 47, 110, 117, 109, 95, 115, 117, 98, 95, 112, 97, 116, 104, 115, 0, 44, 105, 0, 0, 0, 0, 0, 1, 0, 0, 0, 56, 47, 115, 121, 109, 98, 111, 108, 47, 49, 47, 115, 117, 98, 115, 121, 109, 98, 111, 108, 47, 50, 47, 112, 97, 116, 104, 47, 48, 47, 115, 116, 114, 0, 0, 0, 0, 44, 115, 0, 0, 109, 32, 50, 32, 50, 32, 108, 32, 50, 57, 53, 32, 50, 0, 0, 0, 0, 0, 0, 48, 47, 115, 121, 109, 98, 111, 108, 47, 49, 47, 115, 117, 98, 115, 121, 109, 98, 111, 108, 47, 50, 47, 112, 97, 116, 104, 47, 48, 47, 108, 101, 110, 103, 116, 104, 0, 44, 100, 0, 0, 64, 114, 80, 0, 0, 0, 0, 0, 0, 0, 0, 52, 47, 115, 121, 109, 98, 111, 108, 47, 49, 47, 115, 117, 98, 115, 121, 109, 98, 111, 108, 47, 50, 47, 112, 97, 116, 104, 47, 48, 47, 116, 105, 109, 101, 47, 115, 116, 97, 114, 116, 0, 44, 100, 0, 0, 63, -108, 122, -31, 71, -82, 20, 123, 0, 0, 0, 56, 47, 115, 121, 109, 98, 111, 108, 47, 49, 47, 115, 117, 98, 115, 121, 109, 98, 111, 108, 47, 50, 47, 112, 97, 116, 104, 47, 48, 47, 116, 105, 109, 101, 47, 100, 117, 114, 97, 116, 105, 111, 110, 0, 0, 44, 100, 0, 0, 64, 7, 112, -93, -41, 10, 61, 113, 0, 0, 0, 36, 47, 115, 121, 109, 98, 111, 108, 47, 49, 47, 115, 117, 98, 115, 121, 109, 98, 111, 108, 47, 50, 47, 102, 105, 108, 108, 0, 0, 44, 105, 0, 0, 0, 0, 0, 0, 0, 0, 0, 52, 47, 115, 121, 109, 98, 111, 108, 47, 49, 47, 115, 117, 98, 115, 121, 109, 98, 111, 108, 47, 50, 47, 115, 116, 114, 111, 107, 101, 47, 116, 104, 105, 99, 107, 110, 101, 115, 115, 0, 0, 44, 100, 0, 0, 64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 44, 47, 115, 121, 109, 98, 111, 108, 47, 49, 47, 115, 117, 98, 115, 121, 109, 98, 111, 108, 47, 51, 47, 110, 97, 109, 101, 0, 0, 44, 115, 0, 0, 116, 114, 97, 106, 101, 99, 116, 111, 114, 121, 0, 0, 0, 0, 0, 40, 47, 115, 121, 109, 98, 111, 108, 47, 49, 47, 115, 117, 98, 115, 121, 109, 98, 111, 108, 47, 51, 47, 116, 121, 112, 101, 0, 0, 44, 115, 0, 0, 112, 97, 116, 104, 0, 0, 0, 0, 0, 0, 0, 52, 47, 115, 121, 109, 98, 111, 108, 47, 49, 47, 115, 117, 98, 115, 121, 109, 98, 111, 108, 47, 51, 47, 105, 100, 0, 0, 0, 0, 44, 115, 0, 0, 116, 114, 97, 106, 101, 99, 116, 111, 114, 121, 47, 112, 97, 108, 101, 116, 116, 101, 0, 0, 0, 0, 0, 40, 47, 115, 121, 109, 98, 111, 108, 47, 49, 47, 115, 117, 98, 115, 121, 109, 98, 111, 108, 47, 51, 47, 115, 116, 97, 102, 102, 0, 44, 115, 0, 0, 60, 110, 111, 110, 101, 62, 0, 0, 0, 0, 0, 36, 47, 115, 121, 109, 98, 111, 108, 47, 49, 47, 115, 117, 98, 115, 121, 109, 98, 111, 108, 47, 51, 47, 120, 0, 44, 100, 0, 0, 64, 38, 0, 0, 0, 0, 0, 0, 0, 0, 0, 36, 47, 115, 121, 109, 98, 111, 108, 47, 49, 47, 115, 117, 98, 115, 121, 109, 98, 111, 108, 47, 51, 47, 121, 0, 44, 100, 0, 0, 64, 34, 0, 0, 0, 0, 0, 0, 0, 0, 0, 36, 47, 115, 121, 109, 98, 111, 108, 47, 49, 47, 115, 117, 98, 115, 121, 109, 98, 111, 108, 47, 51, 47, 119, 0, 44, 100, 0, 0, 64, 61, 0, 0, 0, 0, 0, 0, 0, 0, 0, 36, 47, 115, 121, 109, 98, 111, 108, 47, 49, 47, 115, 117, 98, 115, 121, 109, 98, 111, 108, 47, 51, 47, 104, 0, 44, 100, 0, 0, 64, 74, 0, 0, 0, 0, 0, 0, 0, 0, 0, 68, 47, 115, 121, 109, 98, 111, 108, 47, 49, 47, 115, 117, 98, 115, 121, 109, 98, 111, 108, 47, 51, 47, 99, 111, 108, 111, 114, 0, 44, 100, 100, 100, 100, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 63, -16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 44, 47, 115, 121, 109, 98, 111, 108, 47, 49, 47, 115, 117, 98, 115, 121, 109, 98, 111, 108, 47, 51, 47, 110, 117, 109, 95, 115, 117, 98, 95, 112, 97, 116, 104, 115, 0, 44, 105, 0, 0, 0, 0, 0, 1, 0, 0, 0, 76, 47, 115, 121, 109, 98, 111, 108, 47, 49, 47, 115, 117, 98, 115, 121, 109, 98, 111, 108, 47, 51, 47, 112, 97, 116, 104, 47, 48, 47, 115, 116, 114, 0, 0, 0, 0, 44, 115, 0, 0, 109, 32, 50, 32, 52, 52, 46, 55, 49, 32, 113, 32, 51, 57, 46, 53, 32, 54, 54, 46, 48, 52, 52, 32, 50, 48, 46, 55, 53, 32, 50, 46, 48, 52, 51, 0, 0, 0, 0, 48, 47, 115, 121, 109, 98, 111, 108, 47, 49, 47, 115, 117, 98, 115, 121, 109, 98, 111, 108, 47, 51, 47, 112, 97, 116, 104, 47, 48, 47, 108, 101, 110, 103, 116, 104, 0, 44, 100, 0, 0, 64, 81, -114, -107, -23, -31, -80, -118, 0, 0, 0, 52, 47, 115, 121, 109, 98, 111, 108, 47, 49, 47, 115, 117, 98, 115, 121, 109, 98, 111, 108, 47, 51, 47, 112, 97, 116, 104, 47, 48, 47, 116, 105, 109, 101, 47, 115, 116, 97, 114, 116, 0, 44, 100, 0, 0, 63, -108, 122, -31, 71, -82, 20, 123, 0, 0, 0, 56, 47, 115, 121, 109, 98, 111, 108, 47, 49, 47, 115, 117, 98, 115, 121, 109, 98, 111, 108, 47, 51, 47, 112, 97, 116, 104, 47, 48, 47, 116, 105, 109, 101, 47, 100, 117, 114, 97, 116, 105, 111, 110, 0, 0, 44, 100, 0, 0, 63, -48, 0, 0, 0, 0, 0, 0, 0, 0, 0, 36, 47, 115, 121, 109, 98, 111, 108, 47, 49, 47, 115, 117, 98, 115, 121, 109, 98, 111, 108, 47, 51, 47, 102, 105, 108, 108, 0, 0, 44, 105, 0, 0, 0, 0, 0, 0, 0, 0, 0, 52, 47, 115, 121, 109, 98, 111, 108, 47, 49, 47, 115, 117, 98, 115, 121, 109, 98, 111, 108, 47, 51, 47, 115, 116, 114, 111, 107, 101, 47, 116, 104, 105, 99, 107, 110, 101, 115, 115, 0, 0, 44, 100, 0, 0, 64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 36, 47, 115, 121, 109, 98, 111, 108, 47, 49, 47, 116, 105, 109, 101, 47, 115, 116, 97, 114, 116, 0, 0, 0, 0, 44, 100, 0, 0, 63, -79, -21, -123, 30, -72, 81, -20, 0, 0, 0, 36, 47, 115, 121, 109, 98, 111, 108, 47, 49, 47, 116, 105, 109, 101, 47, 100, 117, 114, 97, 116, 105, 111, 110, 0, 44, 100, 0, 0, 64, 7, -62, -113, 92, 40, -11, -61 ],
									"saved_bundle_length" : 4168,
									"text" : "/symbol/0/name : \"spat\",\n/symbol/0/type : \"staff\",\n/symbol/0/id : \"spat_1\",\n/symbol/0/staff : \"<none>\",\n/symbol/0/x : 83.,\n/symbol/0/y : 234.,\n/symbol/0/w : 475.,\n/symbol/0/h : 4.,\n/symbol/0/color : [0., 0., 0., 1.],\n/symbol/0/staffSymbol/name : \"path\",\n/symbol/0/staffSymbol/type : \"path\",\n/symbol/0/staffSymbol/id : \"path_palette\",\n/symbol/0/staffSymbol/staff : \"<none>\",\n/symbol/0/staffSymbol/x : 0.,\n/symbol/0/staffSymbol/y : 0.,\n/symbol/0/staffSymbol/w : 475.,\n/symbol/0/staffSymbol/h : 4.,\n/symbol/0/staffSymbol/color : [0., 0., 0., 1.],\n/symbol/0/staffSymbol/num_sub_paths : 1,\n/symbol/0/staffSymbol/path/0/str : \"m 2 2 l 473 2\",\n/symbol/0/staffSymbol/path/0/length : 471.,\n/symbol/0/staffSymbol/path/0/time/start : 0.02,\n/symbol/0/staffSymbol/path/0/time/duration : 4.71,\n/symbol/0/staffSymbol/fill : 0,\n/symbol/0/staffSymbol/stroke/thickness : 2.,\n/symbol/0/time/start : 0.,\n/symbol/0/time/duration : 4.75,\n/symbol/0/staffSymbol/num_sub_paths : 1,\n/symbol/0/staffSymbol/path/0/str : \"m 2 2 l 473 2\",\n/symbol/0/staffSymbol/path/0/length : 471.,\n/symbol/0/staffSymbol/path/0/time/start : 0.02,\n/symbol/0/staffSymbol/path/0/time/duration : 4.71,\n/symbol/0/staffSymbol/fill : 0,\n/symbol/0/staffSymbol/stroke/thickness : 2.,\n/symbol/1/name : \"trajectory_event\",\n/symbol/1/type : \"group\",\n/symbol/1/id : \"group/palette\",\n/symbol/1/staff : \"spat_1\",\n/symbol/1/x : 90.,\n/symbol/1/y : 111.,\n/symbol/1/w : 297.,\n/symbol/1/h : 76.,\n/symbol/1/color : [0., 0., 0., 1.],\n/symbol/1/numsymbols : 3,\n/symbol/1/subsymbol/1/name : \"frame\",\n/symbol/1/subsymbol/1/type : \"rectangle\",\n/symbol/1/subsymbol/1/id : \"frame/palette\",\n/symbol/1/subsymbol/1/staff : \"<none>\",\n/symbol/1/subsymbol/1/x : 0.,\n/symbol/1/subsymbol/1/y : 36.5,\n/symbol/1/subsymbol/1/w : 85.,\n/symbol/1/subsymbol/1/h : 73.,\n/symbol/1/subsymbol/1/color : [0., 0., 0., 1.],\n/symbol/1/subsymbol/1/fill : 0,\n/symbol/1/subsymbol/1/stroke/thickness : 2.,\n/symbol/1/subsymbol/1/rotation : 0.,\n/symbol/1/subsymbol/2/name : \"duration\",\n/symbol/1/subsymbol/2/type : \"path\",\n/symbol/1/subsymbol/2/id : \"duration/palette\",\n/symbol/1/subsymbol/2/staff : \"<none>\",\n/symbol/1/subsymbol/2/x : 0.,\n/symbol/1/subsymbol/2/y : 72.,\n/symbol/1/subsymbol/2/w : 297.,\n/symbol/1/subsymbol/2/h : 4.,\n/symbol/1/subsymbol/2/color : [0., 0., 0., 1.],\n/symbol/1/subsymbol/2/num_sub_paths : 1,\n/symbol/1/subsymbol/2/path/0/str : \"m 2 2 l 295 2\",\n/symbol/1/subsymbol/2/path/0/length : 293.,\n/symbol/1/subsymbol/2/path/0/time/start : 0.02,\n/symbol/1/subsymbol/2/path/0/time/duration : 2.93,\n/symbol/1/subsymbol/2/fill : 0,\n/symbol/1/subsymbol/2/stroke/thickness : 2.,\n/symbol/1/subsymbol/3/name : \"trajectory\",\n/symbol/1/subsymbol/3/type : \"path\",\n/symbol/1/subsymbol/3/id : \"trajectory/palette\",\n/symbol/1/subsymbol/3/staff : \"<none>\",\n/symbol/1/subsymbol/3/x : 11.,\n/symbol/1/subsymbol/3/y : 9.,\n/symbol/1/subsymbol/3/w : 29.,\n/symbol/1/subsymbol/3/h : 52.,\n/symbol/1/subsymbol/3/color : [0., 0., 0., 1.],\n/symbol/1/subsymbol/3/num_sub_paths : 1,\n/symbol/1/subsymbol/3/path/0/str : \"m 2 44.71 q 39.5 66.044 20.75 2.043\",\n/symbol/1/subsymbol/3/path/0/length : 70.2279,\n/symbol/1/subsymbol/3/path/0/time/start : 0.02,\n/symbol/1/subsymbol/3/path/0/time/duration : 0.25,\n/symbol/1/subsymbol/3/fill : 0,\n/symbol/1/subsymbol/3/stroke/thickness : 2.,\n/symbol/1/time/start : 0.07,\n/symbol/1/time/duration : 2.97"
								}

							}
, 							{
								"box" : 								{
									"comment" : "",
									"id" : "obj-20",
									"index" : 2,
									"maxclass" : "inlet",
									"numinlets" : 0,
									"numoutlets" : 1,
									"outlettype" : [ "FullPacket" ],
									"patching_rect" : [ 573.0, 40.0, 30.0, 30.0 ],
									"style" : ""
								}

							}
, 							{
								"box" : 								{
									"comment" : "",
									"id" : "obj-21",
									"index" : 1,
									"maxclass" : "outlet",
									"numinlets" : 1,
									"numoutlets" : 0,
									"patching_rect" : [ 50.0, 2046.0, 30.0, 30.0 ],
									"style" : ""
								}

							}
 ],
						"lines" : [ 							{
								"patchline" : 								{
									"destination" : [ "obj-18", 0 ],
									"source" : [ "obj-1", 0 ]
								}

							}
, 							{
								"patchline" : 								{
									"destination" : [ "obj-21", 0 ],
									"source" : [ "obj-18", 0 ]
								}

							}
, 							{
								"patchline" : 								{
									"destination" : [ "obj-18", 1 ],
									"source" : [ "obj-20", 0 ]
								}

							}
 ]
					}
,
					"patching_rect" : [ 313.0, 182.5, 50.0, 22.0 ],
					"saved_object_attributes" : 					{
						"description" : "",
						"digest" : "",
						"globalpatchername" : "",
						"style" : "",
						"tags" : ""
					}
,
					"style" : "",
					"text" : "p score"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-21",
					"maxclass" : "newobj",
					"numinlets" : 1,
					"numoutlets" : 2,
					"outlettype" : [ "FullPacket", "FullPacket" ],
					"patching_rect" : [ 283.0, 100.5, 80.0, 22.0 ],
					"style" : "",
					"text" : "symbolist"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-23",
					"maxclass" : "newobj",
					"numinlets" : 1,
					"numoutlets" : 2,
					"outlettype" : [ "", "FullPacket" ],
					"patching_rect" : [ 88.0, 138.0, 136.0, 22.0 ],
					"style" : "",
					"text" : "o.route /staff/spat/event"
				}

			}
, 			{
				"box" : 				{
					"format" : 6,
					"id" : "obj-25",
					"maxclass" : "flonum",
					"numinlets" : 1,
					"numoutlets" : 2,
					"outlettype" : [ "", "bang" ],
					"parameter_enable" : 0,
					"patching_rect" : [ 283.0, 18.0, 50.0, 22.0 ],
					"style" : ""
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-27",
					"maxclass" : "message",
					"numinlets" : 2,
					"numoutlets" : 1,
					"outlettype" : [ "" ],
					"patching_rect" : [ 283.0, 55.0, 50.0, 22.0 ],
					"style" : "",
					"text" : "time $1"
				}

			}
, 			{
				"box" : 				{
					"fontface" : 0,
					"fontsize" : 12.0,
					"id" : "obj-28",
					"linecount" : 2,
					"maxclass" : "o.display",
					"numinlets" : 1,
					"numoutlets" : 1,
					"outlettype" : [ "" ],
					"patching_rect" : [ 88.0, 697.0, 474.0, 48.0 ],
					"text" : "/trajectory_event/trajectory/lookup/xy : [0.0431991, 0.649481],\n/time/ratio : 0.16835"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-4",
					"maxclass" : "newobj",
					"numinlets" : 4,
					"numoutlets" : 0,
					"patching_rect" : [ 279.25, 611.0, 77.0, 22.0 ],
					"style" : "",
					"text" : "dac~ 1 2 3 4"
				}

			}
, 			{
				"box" : 				{
					"channels" : 4,
					"id" : "obj-3",
					"maxclass" : "live.gain~",
					"numinlets" : 4,
					"numoutlets" : 7,
					"outlettype" : [ "signal", "signal", "signal", "signal", "", "float", "list" ],
					"parameter_enable" : 1,
					"patching_rect" : [ 279.25, 498.0, 100.0, 77.0 ],
					"saved_attribute_attributes" : 					{
						"valueof" : 						{
							"parameter_longname" : "live.gain~",
							"parameter_shortname" : "live.gain~",
							"parameter_type" : 0,
							"parameter_mmin" : -70.0,
							"parameter_mmax" : 6.0,
							"parameter_initial" : [ 0.0 ],
							"parameter_unitstyle" : 4
						}

					}
,
					"varname" : "live.gain~"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-2",
					"linecount" : 4,
					"maxclass" : "newobj",
					"numinlets" : 1,
					"numoutlets" : 3,
					"outlettype" : [ "", "", "" ],
					"patching_rect" : [ 417.0, 331.0, 100.0, 62.0 ],
					"saved_object_attributes" : 					{
						"parameter_enable" : 0
					}
,
					"style" : "",
					"text" : "spat.oper @numsources 1 @numspeakers 4"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-1",
					"linecount" : 4,
					"maxclass" : "newobj",
					"numinlets" : 1,
					"numoutlets" : 5,
					"outlettype" : [ "signal", "signal", "signal", "signal", "" ],
					"patching_rect" : [ 283.0, 421.0, 112.0, 62.0 ],
					"saved_object_attributes" : 					{
						"parameter_enable" : 0
					}
,
					"style" : "",
					"text" : "spat.spat~ @numsources 1 @numspeakers 4 @panning angular"
				}

			}
 ],
		"lines" : [ 			{
				"patchline" : 				{
					"destination" : [ "obj-3", 3 ],
					"source" : [ "obj-1", 3 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-3", 2 ],
					"source" : [ "obj-1", 2 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-3", 1 ],
					"source" : [ "obj-1", 1 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-3", 0 ],
					"source" : [ "obj-1", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-21", 0 ],
					"source" : [ "obj-13", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-21", 0 ],
					"source" : [ "obj-15", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-31", 0 ],
					"order" : 0,
					"source" : [ "obj-17", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-41", 0 ],
					"order" : 1,
					"source" : [ "obj-17", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-21", 0 ],
					"midpoints" : [ 322.5, 246.5, 411.0, 246.5, 411.0, 90.5, 292.5, 90.5 ],
					"source" : [ "obj-18", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-20", 0 ],
					"source" : [ "obj-19", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-1", 0 ],
					"source" : [ "obj-2", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-18", 0 ],
					"source" : [ "obj-20", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-20", 1 ],
					"order" : 1,
					"source" : [ "obj-21", 1 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-23", 0 ],
					"source" : [ "obj-21", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-38", 0 ],
					"order" : 0,
					"source" : [ "obj-21", 1 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-17", 0 ],
					"source" : [ "obj-23", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-27", 0 ],
					"source" : [ "obj-25", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-21", 0 ],
					"source" : [ "obj-27", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-4", 3 ],
					"source" : [ "obj-3", 3 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-4", 2 ],
					"source" : [ "obj-3", 2 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-4", 1 ],
					"source" : [ "obj-3", 1 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-4", 0 ],
					"source" : [ "obj-3", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-28", 0 ],
					"source" : [ "obj-31", 0 ]
				}

			}
 ],
		"parameters" : 		{
			"obj-3" : [ "live.gain~", "live.gain~", 0 ]
		}
,
		"dependency_cache" : [ 			{
				"name" : "o.gather.select.maxpat",
				"bootpath" : "~/Documents/dev-lib/CNMAT-odot/patchers/namespace",
				"patcherrelativepath" : "../../../CNMAT-odot/patchers/namespace",
				"type" : "JSON",
				"implicit" : 1
			}
, 			{
				"name" : "spat.spat~.mxo",
				"type" : "iLaX"
			}
, 			{
				"name" : "spat.oper.mxo",
				"type" : "iLaX"
			}
, 			{
				"name" : "o.display.mxo",
				"type" : "iLaX"
			}
, 			{
				"name" : "o.route.mxo",
				"type" : "iLaX"
			}
, 			{
				"name" : "symbolist.mxo",
				"type" : "iLaX"
			}
, 			{
				"name" : "o.compose.mxo",
				"type" : "iLaX"
			}
, 			{
				"name" : "o.explode.mxo",
				"type" : "iLaX"
			}
, 			{
				"name" : "o.downcast.mxo",
				"type" : "iLaX"
			}
, 			{
				"name" : "o.flatten.mxo",
				"type" : "iLaX"
			}
, 			{
				"name" : "o.expr.codebox.mxo",
				"type" : "iLaX"
			}
, 			{
				"name" : "o.select.mxo",
				"type" : "iLaX"
			}
, 			{
				"name" : "o.collect.mxo",
				"type" : "iLaX"
			}
, 			{
				"name" : "o.if.mxo",
				"type" : "iLaX"
			}
 ],
		"autosave" : 0,
		"bgfillcolor_type" : "gradient",
		"bgfillcolor_color1" : [ 0.376471, 0.384314, 0.4, 1.0 ],
		"bgfillcolor_color2" : [ 0.290196, 0.309804, 0.301961, 1.0 ],
		"bgfillcolor_color" : [ 0.290196, 0.309804, 0.301961, 1.0 ],
		"bgfillcolor_angle" : 270.0,
		"bgfillcolor_proportion" : 0.39
	}

}
