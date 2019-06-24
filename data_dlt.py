class data_dlt(object) :
    import numpy as np
    data_dlt = np.array (
        [[15001,8,9,19,27,30,6,7],
        [15002,3,14,20,26,32,6,10],
        [15003,12,19,27,28,34,1,12],
        [15004,3,12,14,18,28,5,6],
        [15005,11,14,15,18,19,7,8],
        [15006,6,8,30,32,33,3,7],
        [15007,2,7,8,14,31,4,8],
        [15008,7,10,11,15,17,3,10],
        [15009,17,22,26,27,33,6,10],
        [15010,2,6,17,30,35,2,7],
        [15011,4,13,22,25,30,1,6],
        [15012,3,4,6,11,31,5,9],
        [15013,11,15,25,28,35,7,10],
        [15014,8,16,25,29,35,3,10],
        [15015,1,5,13,17,32,4,11],
        [15016,6,7,16,20,34,9,11],
        [15017,7,13,16,17,29,1,7],
        [15018,4,7,10,23,31,3,7],
        [15019,2,8,16,22,24,1,3],
        [15020,11,26,28,29,32,1,4],
        [15021,22,23,24,26,31,5,6],
        [15022,9,11,14,15,26,11,12],
        [15023,6,9,11,12,22,3,11],
        [15024,2,6,8,16,34,3,4],
        [15025,7,10,20,22,27,10,11],
        [15026,11,13,14,15,22,2,8],
        [15027,5,21,22,25,28,3,10],
        [15028,3,9,28,32,33,9,10],
        [15029,7,10,19,30,35,3,5],
        [15030,9,18,19,26,31,2,5],
        [15031,7,21,29,32,33,4,12],
        [15032,6,21,25,27,30,7,11],
        [15033,3,17,22,26,28,3,7],
        [15034,4,8,15,18,26,5,9],
        [15035,22,24,29,30,35,1,10],
        [15036,14,18,20,23,24,4,8],
        [15037,10,16,21,28,31,7,8],
        [15038,12,19,22,31,35,7,8],
        [15039,3,6,7,16,25,1,6],
        [15040,2,9,10,23,35,9,12],
        [15041,1,11,21,23,28,1,10],
        [15042,3,7,12,17,24,2,11],
        [15043,2,20,22,25,29,2,7],
        [15044,16,18,27,31,33,1,3],
        [15045,8,9,22,29,30,8,12],
        [15046,5,7,10,22,26,4,10],
        [15047,1,2,17,23,31,5,12],
        [15048,10,11,22,24,33,2,9],
        [15049,8,9,11,15,22,8,10],
        [15050,6,7,10,20,28,2,7],
        [15051,1,2,11,16,21,1,5],
        [15052,13,19,24,27,29,7,8],
        [15053,8,25,28,29,32,4,7],
        [15054,2,9,15,24,34,3,9],
        [15055,6,11,14,20,30,5,10],
        [15056,6,12,19,26,35,3,10],
        [15057,1,5,7,14,26,4,6],
        [15058,10,14,20,27,30,7,12],
        [15059,7,9,19,23,32,1,3],
        [15060,16,24,25,26,33,4,11],
        [15061,6,7,8,16,22,2,10],
        [15062,6,7,13,14,15,1,4],
        [15063,1,4,6,34,35,2,4],
        [15064,2,8,20,28,31,7,10],
        [15065,4,5,7,13,35,8,11],
        [15066,1,3,7,13,29,2,10],
        [15067,4,10,18,24,27,4,11],
        [15068,2,3,20,31,35,4,8],
        [15069,2,9,11,22,25,1,7],
        [15070,3,9,12,17,18,4,12],
        [15071,4,6,13,16,34,4,10],
        [15072,19,20,21,31,33,9,10],
        [15073,9,26,27,30,34,1,6],
        [15074,1,12,21,27,33,4,9],
        [15075,13,14,26,27,33,10,12],
        [15076,3,19,21,31,34,5,12],
        [15077,1,16,17,22,28,2,9],
        [15078,19,24,26,28,33,1,2],
        [15079,1,9,17,23,24,4,5],
        [15080,14,20,29,30,32,3,8],
        [15081,3,4,10,13,17,5,9],
        [15082,8,15,28,31,33,3,6],
        [15083,14,16,19,25,29,10,12],
        [15084,10,20,21,32,34,6,7],
        [15085,2,3,4,6,16,3,12],
        [15086,5,9,11,15,32,4,9],
        [15087,2,5,15,17,22,1,11],
        [15088,5,15,28,31,34,11,12],
        [15089,5,12,18,20,27,10,12],
        [15090,6,9,11,17,21,6,8],
        [15091,2,7,28,29,30,3,7],
        [15092,2,14,21,27,34,5,8],
        [15093,16,17,25,26,27,2,4],
        [15094,2,3,10,11,29,3,7],
        [15095,8,14,26,34,35,5,10],
        [15096,3,9,12,18,21,2,6],
        [15097,4,16,17,26,27,1,4],
        [15098,9,14,21,27,33,6,9],
        [15099,9,12,14,22,25,5,10],
        [15100,9,15,18,23,32,5,7],
        [15101,6,12,17,18,32,7,12],
        [15102,11,14,15,33,34,3,12],
        [15103,7,9,15,25,34,4,10],
        [15104,16,19,25,30,34,4,9],
        [15105,8,9,15,21,27,5,9],
        [15106,2,11,13,22,30,2,3],
        [15107,3,23,25,28,29,6,7],
        [15108,1,13,16,26,31,7,12],
        [15109,1,6,11,22,31,3,12],
        [15110,2,7,10,22,30,7,11],
        [15111,7,8,16,24,28,3,10],
        [15112,4,5,15,19,28,4,11],
        [15113,1,14,21,29,33,2,10],
        [15114,9,18,24,31,34,2,10],
        [15115,8,12,13,14,16,3,9],
        [15116,7,10,19,24,33,3,7],
        [15117,6,17,20,30,33,10,11],
        [15118,3,7,20,24,31,4,9],
        [15119,1,8,12,16,21,6,11],
        [15120,1,9,15,24,32,5,10],
        [15121,2,4,17,27,28,4,6],
        [15122,11,13,15,26,35,6,11],
        [15123,4,15,27,29,30,2,10],
        [15124,4,6,12,31,32,3,6],
        [15125,2,3,15,17,28,8,10],
        [15126,4,5,8,9,21,3,6],
        [15127,9,11,14,19,30,3,8],
        [15128,10,12,23,30,32,3,4],
        [15129,1,6,23,25,29,3,9],
        [15130,17,25,31,32,34,2,],
        [15131,11,17,19,20,27,2,3],
        [15132,3,8,14,18,27,1,5],
        [15133,3,7,8,25,30,4,8],
        [15134,1,2,6,15,35,4,7],
        [15135,5,16,20,25,29,6,8],
        [15136,5,20,23,29,32,2,10],
        [15137,14,17,21,23,26,8,11],
        [15138,1,16,20,29,32,4,7],
        [15139,2,3,5,25,32,5,7],
        [15140,1,11,15,26,35,6,11],
        [15141,2,4,15,19,31,2,3],
        [15142,12,14,22,26,30,3,6],
        [15143,9,20,31,33,35,10,11],
        [15144,1,9,24,26,29,6,11],
        [15145,6,7,20,29,32,11,12],
        [15146,4,15,19,27,31,4,11],
        [15147,1,2,13,18,19,8,10],
        [15148,8,11,27,31,33,8,9],
        [15149,4,6,10,12,28,2,8],
        [15150,4,13,18,25,33,3,7],
        [15151,15,16,18,30,35,1,7],
        [5152,4,5,22,29,35,2,11],
        [15153,1,7,22,27,33,2,11],
        [16001,1,7,19,20,30,5,10],
        [16002,7,12,18,25,35,7,9],
        [16003,4,12,17,22,26,6,7],
        [16004,10,20,23,32,35,9,10],
        [16005,6,9,12,15,20,8,11],
        [16006,14,16,17,22,24,2,9],
        [16007,2,5,14,32,35,1,3],
        [16008,5,12,31,34,35,4,5],
        [16009,5,8,10,17,20,2,12],
        [16010,12,13,15,16,28,2,5],
        [16011,2,8,10,15,23,5,10],
        [16012,5,7,11,23,28,3,12],
        [16013,3,21,23,31,35,2,11],
        [16014,6,8,21,24,27,2,10],
        [16015,1,4,10,13,30,3,9],
        [16016,3,10,22,29,32,2,5],
        [16017,11,18,24,28,29,5,10],
        [16018,5,10,13,27,33,8,11],
        [16019,4,11,15,20,26,8,9],
        [16020,1,16,22,29,30,2,9],
        [16021,1,4,9,10,35,9,11],
        [16022,4,18,21,30,35,2,11],
        [16023,4,9,14,25,27,6,11],
        [16024,1,2,11,15,18,4,8],
        [16025,3,6,13,23,24,7,10],
        [16026,2,6,29,30,34,1,7],
        [16027,6,11,13,15,28,2,6],
        [16028,13,18,20,23,31,3,6],
        [16029,20,26,27,29,32,5,12],
        [16030,10,19,21,29,33,4,6],
        [16031,3,14,24,27,29,4,11],
        [16032,5,9,14,20,33,5,12],
        [16033,3,14,15,31,35,9,10],
        [16034,14,18,31,32,35,9,10],
        [16035,17,22,24,25,29,9,12],
        [16036,7,15,26,30,31,3,6],
        [16037,3,5,13,24,34,3,5],
        [16038,3,7,9,15,33,5,12],
        [16039,1,8,16,22,28,4,5],
        [16040,20,22,29,30,35,1,6],
        [16041,12,23,24,27,34,3,6],
        [16042,2,6,8,19,24,1,6],
        [16043,1,9,16,33,34,9,12],
        [16044,1,6,10,20,31,5,11],
        [16045,7,11,25,30,33,2,8],
        [16046,3,19,22,32,34,8,12],
        [16047,2,3,23,29,32,5,9],
        [16048,4,12,17,25,27,4,10],
        [16049,3,4,5,27,33,4,9],
        [16050,7,17,21,29,35,4,5],
        [16051,16,21,23,34,32,7,8],
        [16052,8,14,19,20,33,10,11],
        [16053,4,13,15,30,35,3,5],
        [16054,2,5,9,21,30,6,7],
        [16055,6,21,27,30,34,4,9],
        [16056,10,12,14,31,35,4,5],
        [16057,8,17,19,23,30,3,8],
        [16058,2,15,17,23,28,5,9],
        [16059,2,10,27,28,33,5,7],
        [16060,2,6,22,27,34,8,11],
        [16061,9,15,17,21,23,7,12],
        [16062,2,10,14,15,35,1,9],
        [16063,8,12,22,27,35,5,9],
        [16064,1,2,10,18,35,5,11],
        [16065,1,9,18,22,24,11,12],
        [16066,10,28,29,31,33,5,11],
        [16067,2,8,11,13,19,2,8],
        [16068,5,7,22,26,34,7,12],
        [16069,14,17,19,23,30,2,9],
        [16070,2,9,13,19,23,5,7],
        [16071,8,15,19,20,33,7,12],
        [16072,16,20,24,26,31,4,9],
        [16073,7,14,17,24,27,8,10],
        [16074,1,5,7,11,14,3,8],
        [16075,3,7,17,22,31,8,12],
        [16076,3,8,10,17,31,5,6],
        [16077,9,11,14,16,35,2,5],
        [16078,7,18,29,31,35,5,11],
        [16079,7,14,15,20,27,5,10],
        [16080,15,20,29,30,33,7,12],
        [16081,6,7,18,21,33,3,8],
        [16082,3,8,23,28,29,2,7],
        [16083,1,5,9,33,35,1,5],
        [16084,8,12,15,26,35,1,8],
        [16085,14,15,27,31,32,1,7],
        [16086,3,5,22,25,30,7,9],
        [16087,3,16,17,23,25,7,9],
        [16088,4,16,17,21,30,6,11],
        [16089,7,14,17,28,34,7,10],
        [16090,4,6,8,28,32,7,12],
        [16091,8,13,17,21,22,2,8],
        [16092,2,13,31,33,34,7,11],
        [16093,1,8,14,30,35,6,7],
        [16094,1,14,16,17,18,5,7],
        [16095,8,12,18,19,21,3,6],
        [16096,1,6,8,12,21,5,10],
        [16097,6,8,18,31,33,10,12],
        [16098,8,9,11,20,29,4,5],
        [16099,3,4,12,14,27,1,5],
        [16100,8,12,19,26,30,2,12],
        [6101,4,6,28,33,35,7,12],
        [16102,3,5,6,34,35,3,12],
        [16103,5,14,20,23,29,3,11],
        [16104,1,20,22,23,30,6,7],
        [16105,8,11,31,34,35,1,2],
        [16106,3,21,30,33,35,1,4],
        [16107,2,6,19,24,26,4,6],
        [16108,10,17,22,23,25,7,9],
        [16109,3,25,28,31,34,11,12],
        [16110,7,20,21,23,30,7,10],
        [16111,1,9,22,26,27,2,12],
        [16112,7,22,23,31,32,4,5],
        [16113,10,11,17,30,34,3,12],
        [16114,16,17,20,32,33,3,7],
        [16115,1,4,15,31,34,2,11],
        [16116,4,6,8,19,34,10,12],
        [16117,3,9,14,16,23,2,9],
        [16118,12,15,18,24,29,3,11],
        [16119,2,3,9,21,22,5,10],
        [16120,3,10,20,32,34,4,7],
        [16121,4,5,8,15,20,1,6],
        [16122,1,3,12,14,29,2,6],
        [16123,4,5,13,20,32,1,10],
        [16124,4,13,24,26,29,8,11],
        [16125,6,7,8,28,30,1,2],
        [16126,11,13,16,19,24,6,8],
        [16127,1,5,15,24,26,9,10],
        [16128,17,20,22,30,33,7,8],
        [16129,7,8,19,30,35,8,9],
        [16130,11,23,32,33,34,1,12],
        [16131,6,13,25,28,30,2,7],
        [16132,4,18,19,22,35,1,2],
        [16133,1,4,5,23,28,5,9],
        [16134,1,3,31,33,34,2,7],
        [16135,1,10,12,18,35,1,12],
        [16136,5,6,7,19,23,4,11],
        [16137,7,20,23,29,34,2,10],
        [16138,12,13,16,29,35,5,11],
        [16139,13,21,30,31,35,1,8],
        [16140,12,16,23,29,34,4,11],
        [16141,2,6,7,19,28,11,12],
        [16142,1,6,17,23,26,6,12],
        [16143,4,6,22,27,31,6,7],
        [16144,4,5,13,22,31,5,10],
        [16145,1,9,11,17,28,3,8],
        [16146,21,22,25,29,34,1,11],
        [16147,7,19,20,28,30,4,12],
        [16148,5,14,15,23,25,6,8],
        [16149,1,2,3,12,33,4,9],
        [16150,1,10,18,24,29,7,10],
        [16151,4,7,11,14,21,5,12],
        [16152,8,17,22,24,30,5,11],
        [16153,13,18,25,29,35,3,11],
        [16154,6,16,17,21,29,4,6],
        [17001,7,9,20,31,33,8,10],
        [17002,7,12,14,31,34,9,11],
        [17003,3,5,6,13,22,9,12],
        [17004,5,8,11,28,31,6,7],
        [17005,5,9,20,26,35,2,11],
        [17006,5,9,20,26,35,2,11],
        [17007,8,11,17,25,27,2,4],
        [17008,3,4,9,25,32,2,4],
        [17009,5,6,8,12,22,3,4],
        [17010,4,10,13,18,35,1,2],
        [17011,5,15,21,29,34,6,9],
        [17012,4,5,24,26,35,7,12],
        [17013,5,12,17,30,34,11,12],
        [17014,4,8,15,24,25,2,5],
        [17015,1,7,19,27,34,1,2],
        [17016,3,13,28,30,33,9,10],
        [17017,4,19,24,26,29,4,6],
        [17018,6,7,10,17,23,1,9],
        [17019,2,6,14,23,25,2,10],
        [17020,16,21,24,27,28,4,7],
        [17021,1,15,17,20,26,6,8],
        [17022,2,3,9,17,24,6,12],
        [17023,5,8,12,24,34,7,10],
        [17024,21,23,29,32,35,11,12],
        [17025,10,16,19,20,34,4,5],
        [17026,10,14,19,25,32,2,7],
        [17027,8,17,25,33,34,3,11],
        [17028,6,18,22,23,33,9,11],
        [17029,1,6,14,17,29,1,11],
        [17030,21,22,24,29,34,5,6],
        [17031,2,16,17,30,32,2,9],
        [17032,2,13,14,18,29,3,9],
        [17033,15,22,23,25,34,5,11],
        [17034,6,12,21,26,29,2,11],
        [17035,14,17,22,30,33,7,12],
        [17036,1,4,9,18,33,2,5],
        [17037,13,15,16,32,34,6,11],
        [17038,4,5,13,18,27,3,9],
        [17039,5,6,14,17,26,3,8],
        [17040,4,6,10,23,29,9,11],
        [17041,2,4,6,25,31,5,9],
        [17042,2,17,31,33,34,1,7],
        [17043,6,13,22,25,35,9,12],
        [17044,11,16,17,18,25,2,9],
        [17045,1,8,11,21,35,9,12],
        [17046,4,17,20,23,30,2,3],
        [17047,15,19,32,33,34,9,12],
        [17048,7,9,13,15,19,3,10],
        [17049,9,12,14,22,31,5,8],
        [17050,12,14,17,20,28,2,6],
        [17051,14,17,18,22,29,10,11],
        [17052,3,19,24,28,33,4,12],
        [17053,6,19,20,22,26,1,11],
        [17054,16,29,30,32,33,4,5],
        [17055,19,20,22,30,33,3,9],
        [17056,1,4,24,27,32,3,10],
        [17057,12,16,19,23,24,2,5],
        [17058,14,18,20,21,28,3,6],
        [17059,8,11,13,15,17,3,10],
        [17060,9,15,20,26,31,5,12],
        [17061,10,19,23,25,30,2,4],
        [17062,1,4,6,8,28,8,12],
        [17063,18,21,22,24,29,5,11],
        [17064,1,8,20,27,30,3,4],
        [17065,5,11,12,19,28,5,12],
        [17066,11,15,23,26,30,2,11],
        [17067,10,17,18,32,35,10,11],
        [17068,7,8,19,24,27,6,7],
        [17069,10,16,20,23,32,8,11],
        [17070,6,16,18,26,30,2,3],
        [17071,1,4,11,14,33,5,7],
        [17072,1,3,29,32,34,2,5],
        [17073,7,18,28,31,33,1,2],
        [17074,8,10,11,13,17,8,11],
        [17075,5,11,16,18,35,1,6],
        [17076,2,7,25,30,35,7,12],
        [17077,1,3,4,25,31,6,9],
        [17078,4,5,27,31,34,3,6],
        [17079,1,17,19,32,33,2,11],
        [17080,8,10,21,30,31,2,5],
        [17081,2,12,15,20,34,1,6],
        [17082,5,13,20,21,25,8,9],
        [17083,2,5,14,22,34,3,9],
        [17084,6,11,14,21,31,9,12],
        [17085,9,13,14,25,30,2,5],
        [17086,12,15,18,22,31,1,8],
        [17087,3,14,21,29,35,2,6],
        [17088,5,28,31,34,35,8,9],
        [17089,3,5,6,15,18,3,10],
        [17090,3,6,15,22,35,9,11],
        [17091,1,5,7,16,19,3,12],
        [17092,3,7,9,17,34,4,5],
        [17093,5,6,17,21,29,2,4],
        [17094,3,5,8,29,33,4,8],
        [17095,15,32,33,34,35,3,12],
        [17096,3,7,9,17,28,4,12],
        [17097,7,14,19,22,34,7,12],
        [17098,6,9,17,30,35,10,11],
        [17099,8,9,17,18,24,3,7],
        [17100,2,5,18,19,31,1,9],
        [17101,5,15,21,25,29,5,8],
        [17102,1,12,23,24,29,8,12],
        [17103,4,19,20,24,29,3,4],
        [17104,9,11,22,27,30,9,11],
        [17105,4,15,22,24,32,2,9],
        [17106,9,12,18,23,29,2,4],
        [17107,4,10,19,25,27,6,12],
        [17108,1,4,15,21,22,4,9],
        [17109,6,9,19,26,34,5,7],
        [17110,10,14,20,21,35,2,5],
        [17111,2,14,17,26,34,8,12],
        [17112,5,6,20,31,32,6,12],
        [17113,5,8,17,18,23,4,12],
        [17114,6,7,12,18,23,1,12],
        [17115,14,19,20,25,31,6,8],
        [17116,2,27,30,32,33,1,3],
        [17117,5,7,9,24,32,8,10],
        [17118,2,7,16,20,33,3,11],
        [17119,5,7,13,29,35,3,8],
        [17120,8,15,24,26,27,5,6],
        [17121,1,6,12,26,31,1,7],
        [17122,3,12,18,29,34,3,11],
        [17123,3,15,23,26,32,1,12],
        [17124,3,29,30,32,35,3,8],
        [17125,7,11,18,26,28,4,5],
        [17126,3,4,13,24,33,1,11],
        [17127,2,15,18,21,22,3,10],
        [17128,9,11,13,18,33,2,3],
        [17129,5,17,20,32,33,4,9],
        [17130,5,18,28,33,34,3,4],
        [17131,3,5,8,19,34,1,12],
        [17132,11,17,23,26,27,1,10],
        [17133,15,17,19,32,33,1,3],
        [17134,7,18,19,32,34,2,10],
        [17135,1,12,15,19,22,2,4],
        [17136,1,11,20,21,22,3,4],
        [17137,1,20,22,28,29,5,7],
        [17138,4,7,9,20,25,2,11],
        [17139,2,6,19,25,32,9,12],
        [17140,10,16,25,26,29,1,5],
        [17141,2,8,21,22,29,3,5],
        [17142,1,5,16,19,28,2,10],
        [17143,6,9,24,29,32,2,7],
        [17144,3,21,23,29,32,10,11],
        [17145,4,7,17,18,19,3,10],
        [17146,1,5,11,14,35,9,10],
        [17147,5,16,17,25,34,1,11],
        [17148,19,27,31,32,35,11,12],
        [17149,10,18,19,25,26,4,12],
        [17150,3,6,10,13,18,9,11],
        [17151,5,6,23,28,32,4,5],
        [17152,2,4,7,23,28,2,8],
        [17153,1,9,24,26,34,9,12]]
        )  
