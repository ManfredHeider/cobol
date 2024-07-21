      *   Mainframe Express 3.1  6.011.0034
      *   Micro Focus BMS Screen Painter (ver IDE_6-0-53)
      *   MapSet Name   BPA0053
      *   Date Created  08/12/2015
      *   Time Created  11:03:48
 
      *  Input Data For Map BPA0053
         01 BPA0053I.
            03 FILLER                         PIC X(12).
            03 AGESL                          PIC S9(4) COMP.
            03 AGESF                          PIC X.
            03 FILLER                         PIC X(1).
            03 AGESI                          PIC X(2).
            03 AGESBML                        PIC S9(4) COMP.
            03 AGESBMF                        PIC X.
            03 FILLER                         PIC X(1).
            03 AGESBMI                        PIC X(10).
            03 ASEIFBL                        PIC S9(4) COMP.
            03 ASEIFBF                        PIC X.
            03 FILLER                         PIC X(1).
            03 ASEIFBI                        PIC X(6).
            03 ESEIAKTL                       PIC S9(4) COMP.
            03 ESEIAKTF                       PIC X.
            03 FILLER                         PIC X(1).
            03 ESEIAKTI                       PIC X(3).
            03 ASEISTRL                       PIC S9(4) COMP.
            03 ASEISTRF                       PIC X.
            03 FILLER                         PIC X(1).
            03 ASEISTRI                       PIC X(1).
            03 ASEIGESL                       PIC S9(4) COMP.
            03 ASEIGESF                       PIC X.
            03 FILLER                         PIC X(1).
            03 ASEIGESI                       PIC X(3).
            03 AABRDAVL                       PIC S9(4) COMP.
            03 AABRDAVF                       PIC X.
            03 FILLER                         PIC X(1).
            03 AABRDAVI                       PIC X(10).
            03 AABRDABL                       PIC S9(4) COMP.
            03 AABRDABF                       PIC X.
            03 FILLER                         PIC X(1).
            03 AABRDABI                       PIC X(10).
            03 AFCFBL                         PIC S9(4) COMP.
            03 AFCFBF                         PIC X.
            03 FILLER                         PIC X(1).
            03 AFCFBI                         PIC X(2).
            03 DFHG001I                       OCCURS 10 TIMES.
               05 EFCZL                          PIC S9(4) COMP.
               05 EFCZF                          PIC X.
               05 FILLER                         PIC X(1).
               05 EFCZI                          PIC X(1).
               05 AVSTATL                        PIC S9(4) COMP.
               05 AVSTATF                        PIC X.
               05 FILLER                         PIC X(1).
               05 AVSTATI                        PIC X(4).
               05 ASANZBWL                       PIC S9(4) COMP.
               05 ASANZBWF                       PIC X.
               05 FILLER                         PIC X(1).
               05 ASANZBWI                       PIC X(7).
               05 ASGUTL                         PIC S9(4) COMP.
               05 ASGUTF                         PIC X.
               05 FILLER                         PIC X(1).
               05 ASGUTI                         PIC X(13).
               05 AUBTGGL                        PIC S9(4) COMP.
               05 AUBTGGF                        PIC X.
               05 FILLER                         PIC X(1).
               05 AUBTGGI                        PIC X(13).
               05 ASBELASL                       PIC S9(4) COMP.
               05 ASBELASF                       PIC X.
               05 FILLER                         PIC X(1).
               05 ASBELASI                       PIC X(13).
               05 AUBTGBL                        PIC S9(4) COMP.
               05 AUBTGBF                        PIC X.
               05 FILLER                         PIC X(1).
               05 AUBTGBI                        PIC X(13).
            03 AFCZANZL                       PIC S9(4) COMP.
            03 AFCZANZF                       PIC X.
            03 FILLER                         PIC X(1).
            03 AFCZANZI                       PIC X(79).
 
      *  Output Data For Map BPA0053
         01 BPA0053O REDEFINES BPA0053I.
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
            03 ASEIFBA                        PIC X.
            03 ASEIFBH                        PIC X.
            03 ASEIFBO                        PIC X(6).
            03 FILLER                         PIC X(2).
            03 ESEIAKTA                       PIC X.
            03 ESEIAKTH                       PIC X.
            03 ESEIAKTO                       PIC X(3).
            03 FILLER                         PIC X(2).
            03 ASEISTRA                       PIC X.
            03 ASEISTRH                       PIC X.
            03 ASEISTRO                       PIC X(1).
            03 FILLER                         PIC X(2).
            03 ASEIGESA                       PIC X.
            03 ASEIGESH                       PIC X.
            03 ASEIGESO                       PIC X(3).
            03 FILLER                         PIC X(2).
            03 AABRDAVA                       PIC X.
            03 AABRDAVH                       PIC X.
            03 AABRDAVO                       PIC X(10).
            03 FILLER                         PIC X(2).
            03 AABRDABA                       PIC X.
            03 AABRDABH                       PIC X.
            03 AABRDABO                       PIC X(10).
            03 FILLER                         PIC X(2).
            03 AFCFBA                         PIC X.
            03 AFCFBH                         PIC X.
            03 AFCFBO                         PIC X(2).
            03 DFHG001O OCCURS 10.
               05 FILLER                         PIC X(2).
               05 EFCZA                          PIC X.
               05 EFCZH                          PIC X.
               05 EFCZO                          PIC X(1).
               05 FILLER                         PIC X(2).
               05 AVSTATA                        PIC X.
               05 AVSTATH                        PIC X.
               05 AVSTATO                        PIC X(4).
               05 FILLER                         PIC X(2).
               05 ASANZBWA                       PIC X.
               05 ASANZBWH                       PIC X.
               05 ASANZBWO                       PIC X(7).
               05 FILLER                         PIC X(2).
               05 ASGUTA                         PIC X.
               05 ASGUTH                         PIC X.
               05 ASGUTO                         PIC X(13).
               05 FILLER                         PIC X(2).
               05 AUBTGGA                        PIC X.
               05 AUBTGGH                        PIC X.
               05 AUBTGGO                        PIC X(13).
               05 FILLER                         PIC X(2).
               05 ASBELASA                       PIC X.
               05 ASBELASH                       PIC X.
               05 ASBELASO                       PIC X(13).
               05 FILLER                         PIC X(2).
               05 AUBTGBA                        PIC X.
               05 AUBTGBH                        PIC X.
               05 AUBTGBO                        PIC X(13).
            03 FILLER                         PIC X(2).
            03 AFCZANZA                       PIC X.
            03 AFCZANZH                       PIC X.
            03 AFCZANZO                       PIC X(79).

