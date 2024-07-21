      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  07.03.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DBVB0050.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DBVB0050.CPv  $
      *
      *   Rev 2.2   Jan 05 2017 11:43:22   t13ae13
      *AVB01437: VB58 - Funktionalität erweitern
      *
      *   Rev 2.1   Apr 05 2011 09:14:52   t13ex43
      *AVB01132: VB58 - Schliessen einer Direktionsleitung
      *
      *   Rev 2.0   Mar 07 2011 13:07:26   T13EX43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *   MAINFRAME EXPRESS 3.1  6.007.0036
      *   MICRO FOCUS BMS SCREEN PAINTER (VER IDE_6-0-31)
      *   MAPSET NAME   BVB0050
      *   DATE CREATED  03/23/2011
      *   TIME CREATED  09:15:43

      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME
      * --------+--------+----+---------------------------+----------
      * AVB01132!07.03.11!2.1 ! NEU                       ! KLEEMEIER
      * AVB01437!03.01.17!2.2 ! Neue Felder               ! Walb

      *  Input Data For Map BVB0050
         01 BVB0050I.
            03 FILLER                         PIC X(12).
            03 ESTNRL                         PIC S9(4) COMP.
            03 ESTNRF                         PIC X.
            03 FILLER REDEFINES ESTNRF.
               05 ESTNRA                         PIC X.
            03 ESTNRI                         PIC X(7).
            03 ESTPRZL                        PIC S9(4) COMP.
            03 ESTPRZF                        PIC X.
            03 FILLER REDEFINES ESTPRZF.
               05 ESTPRZA                        PIC X.
            03 ESTPRZI                        PIC X(1).
            03 ANAME1L                        PIC S9(4) COMP.
            03 ANAME1F                        PIC X.
            03 FILLER REDEFINES ANAME1F.
               05 ANAME1A                        PIC X.
            03 ANAME1I                        PIC X(30).
            03 ASTR1L                         PIC S9(4) COMP.
            03 ASTR1F                         PIC X.
            03 FILLER REDEFINES ASTR1F.
               05 ASTR1A                         PIC X.
            03 ASTR1I                         PIC X(30).
            03 AORT1L                         PIC S9(4) COMP.
            03 AORT1F                         PIC X.
            03 FILLER REDEFINES AORT1F.
               05 AORT1A                         PIC X.
            03 AORT1I                         PIC X(30).
            03 ADIR1L                         PIC S9(4) COMP.
            03 ADIR1F                         PIC X.
            03 FILLER REDEFINES ADIR1F.
               05 ADIR1A                         PIC X.
            03 ADIR1I                         PIC X(3).
            03 ABTRNAML                       PIC S9(4) COMP.
            03 ABTRNAMF                       PIC X.
            03 FILLER REDEFINES ABTRNAMF.
               05 ABTRNAMA                       PIC X.
            03 ABTRNAMI                       PIC X(45).
            03 EAUSL                          PIC S9(4) COMP.
            03 EAUSF                          PIC X.
            03 FILLER REDEFINES EAUSF.
               05 EAUSA                          PIC X.
            03 EAUSI                          PIC X(1).
            03 ETAETL                         PIC S9(4) COMP.
            03 ETAETF                         PIC X.
            03 FILLER REDEFINES ETAETF.
               05 ETAETA                         PIC X.
            03 ETAETI                         PIC X(1).
            03 EDIRL                          PIC S9(4) COMP.
            03 EDIRF                          PIC X.
            03 FILLER REDEFINES EDIRF.
               05 EDIRA                          PIC X.
            03 EDIRI                          PIC X(3).
            03 AABDATL                        PIC S9(4) COMP.
            03 AABDATF                        PIC X.
            03 FILLER REDEFINES AABDATF.
               05 AABDATA                        PIC X.
            03 AABDATI                        PIC X(10).
            03 ADIRALTL                       PIC S9(4) COMP.
            03 ADIRALTF                       PIC X.
            03 FILLER REDEFINES ADIRALTF.
               05 ADIRALTA                       PIC X.
            03 ADIRALTI                       PIC X(3).
            03 EDIRFRL                        PIC S9(4) COMP.
            03 EDIRFRF                        PIC X.
            03 FILLER REDEFINES EDIRFRF.
               05 EDIRFRA                        PIC X.
            03 EDIRFRI                        PIC X(1).

      *  Output Data For Map BVB0050
         01 BVB0050O REDEFINES BVB0050I.
            03 FILLER                         PIC X(12).
            03 FILLER                         PIC X(3).
            03 ESTNRO                         PIC X(7).
            03 FILLER                         PIC X(3).
            03 ESTPRZO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 ANAME1O                        PIC X(30).
            03 FILLER                         PIC X(3).
            03 ASTR1O                         PIC X(30).
            03 FILLER                         PIC X(3).
            03 AORT1O                         PIC X(30).
            03 FILLER                         PIC X(3).
            03 ADIR1O                         PIC X(3).
            03 FILLER                         PIC X(3).
            03 ABTRNAMO                       PIC X(45).
            03 FILLER                         PIC X(3).
            03 EAUSO                          PIC X(1).
            03 FILLER                         PIC X(3).
            03 ETAETO                         PIC X(1).
            03 FILLER                         PIC X(3).
            03 EDIRO                          PIC X(3).
            03 FILLER                         PIC X(3).
            03 AABDATO                        PIC X(10).
            03 FILLER                         PIC X(3).
            03 ADIRALTO                       PIC X(3).
            03 FILLER                         PIC X(3).
            03 EDIRFRO                        PIC X(1).

