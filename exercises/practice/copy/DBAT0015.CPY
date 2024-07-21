      *   Micro Focus Mainframe Express  3.0.75
      *   Micro Focus BMS Screen Painter (ver COBC_1-1-37-58)
      *   MapSet Name   BAT0015
      *   Date Created  03/23/2009
      *   Time Created  08:09:42

      *  Input Data For Map BAT0015
         01 BAT0015I.
            03 FILLER                         PIC X(12).
            03 AGESL                          PIC S9(4) COMP.
            03 AGESF                          PIC X.
            03 FILLER                         PIC X(1).
            03 AGESI                          PIC X(2).
            03 AGESBML                        PIC S9(4) COMP.
            03 AGESBMF                        PIC X.
            03 FILLER                         PIC X(1).
            03 AGESBMI                        PIC X(10).
            03 APRODL                         PIC S9(4) COMP.
            03 APRODF                         PIC X.
            03 FILLER                         PIC X(1).
            03 APRODI                         PIC X(3).
            03 APRODBZL                       PIC S9(4) COMP.
            03 APRODBZF                       PIC X.
            03 FILLER                         PIC X(1).
            03 APRODBZI                       PIC X(40).
            03 APRODKZL                       PIC S9(4) COMP.
            03 APRODKZF                       PIC X.
            03 FILLER                         PIC X(1).
            03 APRODKZI                       PIC X(10).
            03 APGUELVL                       PIC S9(4) COMP.
            03 APGUELVF                       PIC X.
            03 FILLER                         PIC X(1).
            03 APGUELVI                       PIC X(10).
            03 APGUELBL                       PIC S9(4) COMP.
            03 APGUELBF                       PIC X.
            03 FILLER                         PIC X(1).
            03 APGUELBI                       PIC X(10).
            03 DFHG001I                       OCCURS 8 TIMES.
               05 AGUELABL                       PIC S9(4) COMP.
               05 AGUELABF                       PIC X.
               05 FILLER                         PIC X(1).
               05 AGUELABI                       PIC X(10).
               05 AGUELBIL                       PIC S9(4) COMP.
               05 AGUELBIF                       PIC X.
               05 FILLER                         PIC X(1).
               05 AGUELBII                       PIC X(10).
               05 AHAFTYPL                       PIC S9(4) COMP.
               05 AHAFTYPF                       PIC X.
               05 FILLER                         PIC X(1).
               05 AHAFTYPI                       PIC X(8).
               05 ASELKRIL                       PIC S9(4) COMP.
               05 ASELKRIF                       PIC X.
               05 FILLER                         PIC X(1).
               05 ASELKRII                       PIC X(2).
               05 AHAFWERL                       PIC S9(4) COMP.
               05 AHAFWERF                       PIC X.
               05 FILLER                         PIC X(1).
               05 AHAFWERI                       PIC X(3).
               05 AHAFWE2L                       PIC S9(4) COMP.
               05 AHAFWE2F                       PIC X.
               05 FILLER                         PIC X(1).
               05 AHAFWE2I                       PIC X(3).
               05 AGEAEAML                       PIC S9(4) COMP.
               05 AGEAEAMF                       PIC X.
               05 FILLER                         PIC X(1).
               05 AGEAEAMI                       PIC X(10).
               05 AGEAEVOL                       PIC S9(4) COMP.
               05 AGEAEVOF                       PIC X.
               05 FILLER                         PIC X(1).
               05 AGEAEVOI                       PIC X(8).

      *  Output Data For Map BAT0015
         01 BAT0015O REDEFINES BAT0015I.
            03 FILLER                         PIC X(12).
            03 FILLER                         PIC X(2).
            03 AGESA                          PIC X.
            03 AGESH                          PIC X.
            03 AGESO                          PIC X(2).
            03 FILLER                         PIC X(2).
            03 AGESBMA                        PIC X.
            03 AGESBMH                        PIC X.
            03 AGESBMO                        PIC X(10).
            03 FILLER                         PIC X(2).
            03 APRODA                         PIC X.
            03 APRODH                         PIC X.
            03 APRODO                         PIC X(3).
            03 FILLER                         PIC X(2).
            03 APRODBZA                       PIC X.
            03 APRODBZH                       PIC X.
            03 APRODBZO                       PIC X(40).
            03 FILLER                         PIC X(2).
            03 APRODKZA                       PIC X.
            03 APRODKZH                       PIC X.
            03 APRODKZO                       PIC X(10).
            03 FILLER                         PIC X(2).
            03 APGUELVA                       PIC X.
            03 APGUELVH                       PIC X.
            03 APGUELVO                       PIC X(10).
            03 FILLER                         PIC X(2).
            03 APGUELBA                       PIC X.
            03 APGUELBH                       PIC X.
            03 APGUELBO                       PIC X(10).
            03 DFHG001O OCCURS 8.
               05 FILLER                         PIC X(2).
               05 AGUELABA                       PIC X.
               05 AGUELABH                       PIC X.
               05 AGUELABO                       PIC X(10).
               05 FILLER                         PIC X(2).
               05 AGUELBIA                       PIC X.
               05 AGUELBIH                       PIC X.
               05 AGUELBIO                       PIC X(10).
               05 FILLER                         PIC X(2).
               05 AHAFTYPA                       PIC X.
               05 AHAFTYPH                       PIC X.
               05 AHAFTYPO                       PIC X(8).
               05 FILLER                         PIC X(2).
               05 ASELKRIA                       PIC X.
               05 ASELKRIH                       PIC X.
               05 ASELKRIO                       PIC X(2).
               05 FILLER                         PIC X(2).
               05 AHAFWERA                       PIC X.
               05 AHAFWERH                       PIC X.
               05 AHAFWERO                       PIC X(3).
               05 FILLER                         PIC X(2).
               05 AHAFWE2A                       PIC X.
               05 AHAFWE2H                       PIC X.
               05 AHAFWE2O                       PIC X(3).
               05 FILLER                         PIC X(2).
               05 AGEAEAMA                       PIC X.
               05 AGEAEAMH                       PIC X.
               05 AGEAEAMO                       PIC X(10).
               05 FILLER                         PIC X(2).
               05 AGEAEVOA                       PIC X.
               05 AGEAEVOH                       PIC X.
               05 AGEAEVOO                       PIC X(8).

