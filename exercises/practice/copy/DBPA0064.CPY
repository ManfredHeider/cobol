      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.07.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DBPA0064.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DBPA0064.CPv  $
      *
      *   Rev 2.4   Aug 27 2015 14:31:42   t13ae23
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: PF13
      *
      *   Rev 2.3   Aug 12 2015 09:57:10   t13ae23
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: PF13
      *
      *   Rev 2.2   Jul 21 2015 11:02:16   t13ae23
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: PF13
      *
      *   Rev 2.1   Jul 21 2015 10:21:32   t13ae23
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: PF13
      *
      *   Rev 2.0   Jul 13 2015 11:25:38   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *   MAINFRAME EXPRESS 3.1  6.013.0041
      *   MICRO FOCUS BMS SCREEN PAINTER (VER IDE_6-0-57)
      *   MAPSET NAME   BPA0064
      *   DATE CREATED  08/07/2015
      *   TIME CREATED  11:43:29

      *  INPUT DATA FOR MAP BPA0064
         01 BPA0064I.
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
            03 AABRVDAL                       PIC S9(4) COMP.
            03 AABRVDAF                       PIC X.
            03 FILLER                         PIC X(1).
            03 AABRVDAI                       PIC X(10).
            03 AABRBDAL                       PIC S9(4) COMP.
            03 AABRBDAF                       PIC X.
            03 FILLER                         PIC X(1).
            03 AABRBDAI                       PIC X(10).
            03 AFCFBL                         PIC S9(4) COMP.
            03 AFCFBF                         PIC X.
            03 FILLER                         PIC X(1).
            03 AFCFBI                         PIC X(2).
            03 DFHG001I                       OCCURS 11 TIMES.
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
               05 ASGUTI                         PIC X(15).
               05 AUMSSTGL                       PIC S9(4) COMP.
               05 AUMSSTGF                       PIC X.
               05 FILLER                         PIC X(1).
               05 AUMSSTGI                       PIC X(15).
               05 ASBELASL                       PIC S9(4) COMP.
               05 ASBELASF                       PIC X.
               05 FILLER                         PIC X(1).
               05 ASBELASI                       PIC X(15).
               05 AUMSSTBL                       PIC S9(4) COMP.
               05 AUMSSTBF                       PIC X.
               05 FILLER                         PIC X(1).
               05 AUMSSTBI                       PIC X(15).
            03 AFCZANZL                       PIC S9(4) COMP.
            03 AFCZANZF                       PIC X.
            03 FILLER                         PIC X(1).
            03 AFCZANZI                       PIC X(79).

      *  OUTPUT DATA FOR MAP BPA0064
         01 BPA0064O REDEFINES BPA0064I.
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
            03 AABRVDAA                       PIC X.
            03 AABRVDAH                       PIC X.
            03 AABRVDAO                       PIC X(10).
            03 FILLER                         PIC X(2).
            03 AABRBDAA                       PIC X.
            03 AABRBDAH                       PIC X.
            03 AABRBDAO                       PIC X(10).
            03 FILLER                         PIC X(2).
            03 AFCFBA                         PIC X.
            03 AFCFBH                         PIC X.
            03 AFCFBO                         PIC X(2).
            03 DFHG001O OCCURS 11.
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
               05 ASGUTO                         PIC X(15).
               05 FILLER                         PIC X(2).
               05 AUMSSTGA                       PIC X.
               05 AUMSSTGH                       PIC X.
               05 AUMSSTGO                       PIC X(15).
               05 FILLER                         PIC X(2).
               05 ASBELASA                       PIC X.
               05 ASBELASH                       PIC X.
               05 ASBELASO                       PIC X(15).
               05 FILLER                         PIC X(2).
               05 AUMSSTBA                       PIC X.
               05 AUMSSTBH                       PIC X.
               05 AUMSSTBO                       PIC X(15).
            03 FILLER                         PIC X(2).
            03 AFCZANZA                       PIC X.
            03 AFCZANZH                       PIC X.
            03 AFCZANZO                       PIC X(79).


