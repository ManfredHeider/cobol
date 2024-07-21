      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  20.09.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DBVB0065.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DBVB0065.CPv  $
      *
      *   Rev 2.1   Jan 17 2020 10:09:26   t13ae56
      *AVB01581: PGM - Paket 2 - Dialoge VB31 + VB49
      *
      *   Rev 2.0   Sep 20 2019 10:51:38   t13ae56
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *   MICRO FOCUS ENTERPRISE DEVELOPER FOR ECLIPSE  4.0.00243
      *   MICRO FOCUS BMS SCREEN PAINTER (VER BMSIDE_4-0-05)
      *   MAPSET NAME   BVB0065
      *   DATE CREATED  09/20/2019
      *   TIME CREATED  10:57:49

      *  INPUT DATA FOR MAP BVB0065
         01 BVB0065I.
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
            03 AINFOZLL                       PIC S9(4) COMP.
            03 AINFOZLF                       PIC X.
            03 FILLER REDEFINES AINFOZLF.
               05 AINFOZLA                       PIC X.
            03 AINFOZLI                       PIC X(65).
            03 ANAMEL                         PIC S9(4) COMP.
            03 ANAMEF                         PIC X.
            03 FILLER REDEFINES ANAMEF.
               05 ANAMEA                         PIC X.
            03 ANAMEI                         PIC X(20).
            03 AVORNAML                       PIC S9(4) COMP.
            03 AVORNAMF                       PIC X.
            03 FILLER REDEFINES AVORNAMF.
               05 AVORNAMA                       PIC X.
            03 AVORNAMI                       PIC X(15).
            03 ABTRNAML                       PIC S9(4) COMP.
            03 ABTRNAMF                       PIC X.
            03 FILLER REDEFINES ABTRNAMF.
               05 ABTRNAMA                       PIC X.
            03 ABTRNAMI                       PIC X(16).
            03 ABVABTL                        PIC S9(4) COMP.
            03 ABVABTF                        PIC X.
            03 FILLER REDEFINES ABVABTF.
               05 ABVABTA                        PIC X.
            03 ABVABTI                        PIC X(2).
            03 ABVABML                        PIC S9(4) COMP.
            03 ABVABMF                        PIC X.
            03 FILLER REDEFINES ABVABMF.
               05 ABVABMA                        PIC X.
            03 ABVABMI                        PIC X(2).
            03 ABVABJL                        PIC S9(4) COMP.
            03 ABVABJF                        PIC X.
            03 FILLER REDEFINES ABVABJF.
               05 ABVABJA                        PIC X.
            03 ABVABJI                        PIC X(4).
            03 ASPERREL                       PIC S9(4) COMP.
            03 ASPERREF                       PIC X.
            03 FILLER REDEFINES ASPERREF.
               05 ASPERREA                       PIC X.
            03 ASPERREI                       PIC X(1).
            03 ESPERREL                       PIC S9(4) COMP.
            03 ESPERREF                       PIC X.
            03 FILLER REDEFINES ESPERREF.
               05 ESPERREA                       PIC X.
            03 ESPERREI                       PIC X(1).
            03 AIBANL                         PIC S9(4) COMP.
            03 AIBANF                         PIC X.
            03 FILLER REDEFINES AIBANF.
               05 AIBANA                         PIC X.
            03 AIBANI                         PIC X(1).
            03 EIBANL                         PIC S9(4) COMP.
            03 EIBANF                         PIC X.
            03 FILLER REDEFINES EIBANF.
               05 EIBANA                         PIC X.
            03 EIBANI                         PIC X(34).
            03 ABLZL                          PIC S9(4) COMP.
            03 ABLZF                          PIC X.
            03 FILLER REDEFINES ABLZF.
               05 ABLZA                          PIC X.
            03 ABLZI                          PIC X(1).
            03 EBLZL                          PIC S9(4) COMP.
            03 EBLZF                          PIC X.
            03 FILLER REDEFINES EBLZF.
               05 EBLZA                          PIC X.
            03 EBLZI                          PIC X(8).
            03 ABANKL                         PIC S9(4) COMP.
            03 ABANKF                         PIC X.
            03 FILLER REDEFINES ABANKF.
               05 ABANKA                         PIC X.
            03 ABANKI                         PIC X(1).
            03 EBANKL                         PIC S9(4) COMP.
            03 EBANKF                         PIC X.
            03 FILLER REDEFINES EBANKF.
               05 EBANKA                         PIC X.
            03 EBANKI                         PIC X(40).
            03 AKTONRL                        PIC S9(4) COMP.
            03 AKTONRF                        PIC X.
            03 FILLER REDEFINES AKTONRF.
               05 AKTONRA                        PIC X.
            03 AKTONRI                        PIC X(1).
            03 EKTONRL                        PIC S9(4) COMP.
            03 EKTONRF                        PIC X.
            03 FILLER REDEFINES EKTONRF.
               05 EKTONRA                        PIC X.
            03 EKTONRI                        PIC X(20).
            03 ABVINHL                        PIC S9(4) COMP.
            03 ABVINHF                        PIC X.
            03 FILLER REDEFINES ABVINHF.
               05 ABVINHA                        PIC X.
            03 ABVINHI                        PIC X(1).
            03 EBVINHL                        PIC S9(4) COMP.
            03 EBVINHF                        PIC X.
            03 FILLER REDEFINES EBVINHF.
               05 EBVINHA                        PIC X.
            03 EBVINHI                        PIC X(40).

      *  OUTPUT DATA FOR MAP BVB0065
         01 BVB0065O REDEFINES BVB0065I.
            03 FILLER                         PIC X(12).
            03 FILLER                         PIC X(3).
            03 ESTNRO                         PIC X(7).
            03 FILLER                         PIC X(3).
            03 ESTPRZO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 AINFOZLO                       PIC X(65).
            03 FILLER                         PIC X(3).
            03 ANAMEO                         PIC X(20).
            03 FILLER                         PIC X(3).
            03 AVORNAMO                       PIC X(15).
            03 FILLER                         PIC X(3).
            03 ABTRNAMO                       PIC X(16).
            03 FILLER                         PIC X(3).
            03 ABVABTO                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 ABVABMO                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 ABVABJO                        PIC X(4).
            03 FILLER                         PIC X(3).
            03 ASPERREO                       PIC X(1).
            03 FILLER                         PIC X(3).
            03 ESPERREO                       PIC X(1).
            03 FILLER                         PIC X(3).
            03 AIBANO                         PIC X(1).
            03 FILLER                         PIC X(3).
            03 EIBANO                         PIC X(34).
            03 FILLER                         PIC X(3).
            03 ABLZO                          PIC X(1).
            03 FILLER                         PIC X(3).
            03 EBLZO                          PIC X(8).
            03 FILLER                         PIC X(3).
            03 ABANKO                         PIC X(1).
            03 FILLER                         PIC X(3).
            03 EBANKO                         PIC X(40).
            03 FILLER                         PIC X(3).
            03 AKTONRO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 EKTONRO                        PIC X(20).
            03 FILLER                         PIC X(3).
            03 ABVINHO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 EBVINHO                        PIC X(40).


