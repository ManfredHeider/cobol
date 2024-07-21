      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  11.07.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DBVB0062.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DBVB0062.CPv  $
      *
      *   Rev 2.2   31 Mar 2020 11:45:42   t13ae56
      *AVB01574: PGM - Nachfolgeregelung Praxis (VB61)
      *
      *   Rev 2.1   Aug 27 2019 08:09:24   t13ae56
      *AVB01574: PGM - Nachfolgeregelung Praxis (VB61)
      *
      *   Rev 2.0   Jul 11 2019 18:00:06   t13ae56
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *   MICRO FOCUS ENTERPRISE DEVELOPER FOR ECLIPSE  4.0.00243
      *   MICRO FOCUS BMS SCREEN PAINTER (VER BMSIDE_4-0-05)
      *   MAPSET NAME   BVB0062
      *   DATE CREATED  10/16/2019
      *   TIME CREATED  09:17:00

      *  INPUT DATA FOR MAP BVB0062
         01 BVB0062I.
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
            03 AINFOZ1L                       PIC S9(4) COMP.
            03 AINFOZ1F                       PIC X.
            03 FILLER REDEFINES AINFOZ1F.
               05 AINFOZ1A                       PIC X.
            03 AINFOZ1I                       PIC X(52).
            03 AVBNAM1L                       PIC S9(4) COMP.
            03 AVBNAM1F                       PIC X.
            03 FILLER REDEFINES AVBNAM1F.
               05 AVBNAM1A                       PIC X.
            03 AVBNAM1I                       PIC X(40).
            03 AVBVOR1L                       PIC S9(4) COMP.
            03 AVBVOR1F                       PIC X.
            03 FILLER REDEFINES AVBVOR1F.
               05 AVBVOR1A                       PIC X.
            03 AVBVOR1I                       PIC X(20).
            03 EAUSWL                         PIC S9(4) COMP.
            03 EAUSWF                         PIC X.
            03 FILLER REDEFINES EAUSWF.
               05 EAUSWA                         PIC X.
            03 EAUSWI                         PIC X(1).
            03 ESTNR2L                        PIC S9(4) COMP.
            03 ESTNR2F                        PIC X.
            03 FILLER REDEFINES ESTNR2F.
               05 ESTNR2A                        PIC X.
            03 ESTNR2I                        PIC X(7).
            03 ESTPZ2L                        PIC S9(4) COMP.
            03 ESTPZ2F                        PIC X.
            03 FILLER REDEFINES ESTPZ2F.
               05 ESTPZ2A                        PIC X.
            03 ESTPZ2I                        PIC X(1).
            03 AINFOZ2L                       PIC S9(4) COMP.
            03 AINFOZ2F                       PIC X.
            03 FILLER REDEFINES AINFOZ2F.
               05 AINFOZ2A                       PIC X.
            03 AINFOZ2I                       PIC X(52).
            03 AVBNAM2L                       PIC S9(4) COMP.
            03 AVBNAM2F                       PIC X.
            03 FILLER REDEFINES AVBNAM2F.
               05 AVBNAM2A                       PIC X.
            03 AVBNAM2I                       PIC X(40).
            03 AVBVOR2L                       PIC S9(4) COMP.
            03 AVBVOR2F                       PIC X.
            03 FILLER REDEFINES AVBVOR2F.
               05 AVBVOR2A                       PIC X.
            03 AVBVOR2I                       PIC X(20).
            03 ENFRML                         PIC S9(4) COMP.
            03 ENFRMF                         PIC X.
            03 FILLER REDEFINES ENFRMF.
               05 ENFRMA                         PIC X.
            03 ENFRMI                         PIC X(1).
            03 EPROJNRL                       PIC S9(4) COMP.
            03 EPROJNRF                       PIC X.
            03 FILLER REDEFINES EPROJNRF.
               05 EPROJNRA                       PIC X.
            03 EPROJNRI                       PIC X(10).
            03 ENRVONTL                       PIC S9(4) COMP.
            03 ENRVONTF                       PIC X.
            03 FILLER REDEFINES ENRVONTF.
               05 ENRVONTA                       PIC X.
            03 ENRVONTI                       PIC X(2).
            03 ENRVONML                       PIC S9(4) COMP.
            03 ENRVONMF                       PIC X.
            03 FILLER REDEFINES ENRVONMF.
               05 ENRVONMA                       PIC X.
            03 ENRVONMI                       PIC X(2).
            03 ENRVONJL                       PIC S9(4) COMP.
            03 ENRVONJF                       PIC X.
            03 FILLER REDEFINES ENRVONJF.
               05 ENRVONJA                       PIC X.
            03 ENRVONJI                       PIC X(4).
            03 ENRENDTL                       PIC S9(4) COMP.
            03 ENRENDTF                       PIC X.
            03 FILLER REDEFINES ENRENDTF.
               05 ENRENDTA                       PIC X.
            03 ENRENDTI                       PIC X(2).
            03 ENRENDML                       PIC S9(4) COMP.
            03 ENRENDMF                       PIC X.
            03 FILLER REDEFINES ENRENDMF.
               05 ENRENDMA                       PIC X.
            03 ENRENDMI                       PIC X(2).
            03 ENRENDJL                       PIC S9(4) COMP.
            03 ENRENDJF                       PIC X.
            03 FILLER REDEFINES ENRENDJF.
               05 ENRENDJA                       PIC X.
            03 ENRENDJI                       PIC X(4).
            03 EMO1L                          PIC S9(4) COMP.
            03 EMO1F                          PIC X.
            03 FILLER REDEFINES EMO1F.
               05 EMO1A                          PIC X.
            03 EMO1I                          PIC X(2).
            03 EVONT1L                        PIC S9(4) COMP.
            03 EVONT1F                        PIC X.
            03 FILLER REDEFINES EVONT1F.
               05 EVONT1A                        PIC X.
            03 EVONT1I                        PIC X(2).
            03 EVONM1L                        PIC S9(4) COMP.
            03 EVONM1F                        PIC X.
            03 FILLER REDEFINES EVONM1F.
               05 EVONM1A                        PIC X.
            03 EVONM1I                        PIC X(2).
            03 EVONJ1L                        PIC S9(4) COMP.
            03 EVONJ1F                        PIC X.
            03 FILLER REDEFINES EVONJ1F.
               05 EVONJ1A                        PIC X.
            03 EVONJ1I                        PIC X(4).
            03 AVON1L                         PIC S9(4) COMP.
            03 AVON1F                         PIC X.
            03 FILLER REDEFINES AVON1F.
               05 AVON1A                         PIC X.
            03 AVON1I                         PIC X(10).
            03 EABBTR1L                       PIC S9(4) COMP.
            03 EABBTR1F                       PIC X.
            03 FILLER REDEFINES EABBTR1F.
               05 EABBTR1A                       PIC X.
            03 EABBTR1I                       PIC X(9).
            03 EMO2L                          PIC S9(4) COMP.
            03 EMO2F                          PIC X.
            03 FILLER REDEFINES EMO2F.
               05 EMO2A                          PIC X.
            03 EMO2I                          PIC X(2).
            03 EVONT2L                        PIC S9(4) COMP.
            03 EVONT2F                        PIC X.
            03 FILLER REDEFINES EVONT2F.
               05 EVONT2A                        PIC X.
            03 EVONT2I                        PIC X(2).
            03 EVONM2L                        PIC S9(4) COMP.
            03 EVONM2F                        PIC X.
            03 FILLER REDEFINES EVONM2F.
               05 EVONM2A                        PIC X.
            03 EVONM2I                        PIC X(2).
            03 EVONJ2L                        PIC S9(4) COMP.
            03 EVONJ2F                        PIC X.
            03 FILLER REDEFINES EVONJ2F.
               05 EVONJ2A                        PIC X.
            03 EVONJ2I                        PIC X(4).
            03 AVON2L                         PIC S9(4) COMP.
            03 AVON2F                         PIC X.
            03 FILLER REDEFINES AVON2F.
               05 AVON2A                         PIC X.
            03 AVON2I                         PIC X(10).
            03 EABBTR2L                       PIC S9(4) COMP.
            03 EABBTR2F                       PIC X.
            03 FILLER REDEFINES EABBTR2F.
               05 EABBTR2A                       PIC X.
            03 EABBTR2I                       PIC X(9).
            03 EMO3L                          PIC S9(4) COMP.
            03 EMO3F                          PIC X.
            03 FILLER REDEFINES EMO3F.
               05 EMO3A                          PIC X.
            03 EMO3I                          PIC X(2).
            03 EVONT3L                        PIC S9(4) COMP.
            03 EVONT3F                        PIC X.
            03 FILLER REDEFINES EVONT3F.
               05 EVONT3A                        PIC X.
            03 EVONT3I                        PIC X(2).
            03 EVONM3L                        PIC S9(4) COMP.
            03 EVONM3F                        PIC X.
            03 FILLER REDEFINES EVONM3F.
               05 EVONM3A                        PIC X.
            03 EVONM3I                        PIC X(2).
            03 EVONJ3L                        PIC S9(4) COMP.
            03 EVONJ3F                        PIC X.
            03 FILLER REDEFINES EVONJ3F.
               05 EVONJ3A                        PIC X.
            03 EVONJ3I                        PIC X(4).
            03 AVON3L                         PIC S9(4) COMP.
            03 AVON3F                         PIC X.
            03 FILLER REDEFINES AVON3F.
               05 AVON3A                         PIC X.
            03 AVON3I                         PIC X(10).
            03 EABBTR3L                       PIC S9(4) COMP.
            03 EABBTR3F                       PIC X.
            03 FILLER REDEFINES EABBTR3F.
               05 EABBTR3A                       PIC X.
            03 EABBTR3I                       PIC X(9).
            03 EMO4L                          PIC S9(4) COMP.
            03 EMO4F                          PIC X.
            03 FILLER REDEFINES EMO4F.
               05 EMO4A                          PIC X.
            03 EMO4I                          PIC X(2).
            03 EVONT4L                        PIC S9(4) COMP.
            03 EVONT4F                        PIC X.
            03 FILLER REDEFINES EVONT4F.
               05 EVONT4A                        PIC X.
            03 EVONT4I                        PIC X(2).
            03 EVONM4L                        PIC S9(4) COMP.
            03 EVONM4F                        PIC X.
            03 FILLER REDEFINES EVONM4F.
               05 EVONM4A                        PIC X.
            03 EVONM4I                        PIC X(2).
            03 EVONJ4L                        PIC S9(4) COMP.
            03 EVONJ4F                        PIC X.
            03 FILLER REDEFINES EVONJ4F.
               05 EVONJ4A                        PIC X.
            03 EVONJ4I                        PIC X(4).
            03 AVON4L                         PIC S9(4) COMP.
            03 AVON4F                         PIC X.
            03 FILLER REDEFINES AVON4F.
               05 AVON4A                         PIC X.
            03 AVON4I                         PIC X(10).
            03 EABBTR4L                       PIC S9(4) COMP.
            03 EABBTR4F                       PIC X.
            03 FILLER REDEFINES EABBTR4F.
               05 EABBTR4A                       PIC X.
            03 EABBTR4I                       PIC X(9).
            03 EMO5L                          PIC S9(4) COMP.
            03 EMO5F                          PIC X.
            03 FILLER REDEFINES EMO5F.
               05 EMO5A                          PIC X.
            03 EMO5I                          PIC X(2).
            03 EVONT5L                        PIC S9(4) COMP.
            03 EVONT5F                        PIC X.
            03 FILLER REDEFINES EVONT5F.
               05 EVONT5A                        PIC X.
            03 EVONT5I                        PIC X(2).
            03 EVONM5L                        PIC S9(4) COMP.
            03 EVONM5F                        PIC X.
            03 FILLER REDEFINES EVONM5F.
               05 EVONM5A                        PIC X.
            03 EVONM5I                        PIC X(2).
            03 EVONJ5L                        PIC S9(4) COMP.
            03 EVONJ5F                        PIC X.
            03 FILLER REDEFINES EVONJ5F.
               05 EVONJ5A                        PIC X.
            03 EVONJ5I                        PIC X(4).
            03 AVON5L                         PIC S9(4) COMP.
            03 AVON5F                         PIC X.
            03 FILLER REDEFINES AVON5F.
               05 AVON5A                         PIC X.
            03 AVON5I                         PIC X(10).
            03 EABBTR5L                       PIC S9(4) COMP.
            03 EABBTR5F                       PIC X.
            03 FILLER REDEFINES EABBTR5F.
               05 EABBTR5A                       PIC X.
            03 EABBTR5I                       PIC X(9).
            03 EMO6L                          PIC S9(4) COMP.
            03 EMO6F                          PIC X.
            03 FILLER REDEFINES EMO6F.
               05 EMO6A                          PIC X.
            03 EMO6I                          PIC X(2).
            03 EVONT6L                        PIC S9(4) COMP.
            03 EVONT6F                        PIC X.
            03 FILLER REDEFINES EVONT6F.
               05 EVONT6A                        PIC X.
            03 EVONT6I                        PIC X(2).
            03 EVONM6L                        PIC S9(4) COMP.
            03 EVONM6F                        PIC X.
            03 FILLER REDEFINES EVONM6F.
               05 EVONM6A                        PIC X.
            03 EVONM6I                        PIC X(2).
            03 EVONJ6L                        PIC S9(4) COMP.
            03 EVONJ6F                        PIC X.
            03 FILLER REDEFINES EVONJ6F.
               05 EVONJ6A                        PIC X.
            03 EVONJ6I                        PIC X(4).
            03 AVON6L                         PIC S9(4) COMP.
            03 AVON6F                         PIC X.
            03 FILLER REDEFINES AVON6F.
               05 AVON6A                         PIC X.
            03 AVON6I                         PIC X(10).
            03 EABBTR6L                       PIC S9(4) COMP.
            03 EABBTR6F                       PIC X.
            03 FILLER REDEFINES EABBTR6F.
               05 EABBTR6A                       PIC X.
            03 EABBTR6I                       PIC X(9).
            03 EZAHL1TL                       PIC S9(4) COMP.
            03 EZAHL1TF                       PIC X.
            03 FILLER REDEFINES EZAHL1TF.
               05 EZAHL1TA                       PIC X.
            03 EZAHL1TI                       PIC X(2).
            03 EZAHL1ML                       PIC S9(4) COMP.
            03 EZAHL1MF                       PIC X.
            03 FILLER REDEFINES EZAHL1MF.
               05 EZAHL1MA                       PIC X.
            03 EZAHL1MI                       PIC X(2).
            03 EZAHL1JL                       PIC S9(4) COMP.
            03 EZAHL1JF                       PIC X.
            03 FILLER REDEFINES EZAHL1JF.
               05 EZAHL1JA                       PIC X.
            03 EZAHL1JI                       PIC X(4).
            03 EZAHL2TL                       PIC S9(4) COMP.
            03 EZAHL2TF                       PIC X.
            03 FILLER REDEFINES EZAHL2TF.
               05 EZAHL2TA                       PIC X.
            03 EZAHL2TI                       PIC X(2).
            03 EZAHL2ML                       PIC S9(4) COMP.
            03 EZAHL2MF                       PIC X.
            03 FILLER REDEFINES EZAHL2MF.
               05 EZAHL2MA                       PIC X.
            03 EZAHL2MI                       PIC X(2).
            03 EZAHL2JL                       PIC S9(4) COMP.
            03 EZAHL2JF                       PIC X.
            03 FILLER REDEFINES EZAHL2JF.
               05 EZAHL2JA                       PIC X.
            03 EZAHL2JI                       PIC X(4).
            03 EZAHL3TL                       PIC S9(4) COMP.
            03 EZAHL3TF                       PIC X.
            03 FILLER REDEFINES EZAHL3TF.
               05 EZAHL3TA                       PIC X.
            03 EZAHL3TI                       PIC X(2).
            03 EZAHL3ML                       PIC S9(4) COMP.
            03 EZAHL3MF                       PIC X.
            03 FILLER REDEFINES EZAHL3MF.
               05 EZAHL3MA                       PIC X.
            03 EZAHL3MI                       PIC X(2).
            03 EZAHL3JL                       PIC S9(4) COMP.
            03 EZAHL3JF                       PIC X.
            03 FILLER REDEFINES EZAHL3JF.
               05 EZAHL3JA                       PIC X.
            03 EZAHL3JI                       PIC X(4).
            03 EZAHL4TL                       PIC S9(4) COMP.
            03 EZAHL4TF                       PIC X.
            03 FILLER REDEFINES EZAHL4TF.
               05 EZAHL4TA                       PIC X.
            03 EZAHL4TI                       PIC X(2).
            03 EZAHL4ML                       PIC S9(4) COMP.
            03 EZAHL4MF                       PIC X.
            03 FILLER REDEFINES EZAHL4MF.
               05 EZAHL4MA                       PIC X.
            03 EZAHL4MI                       PIC X(2).
            03 EZAHL4JL                       PIC S9(4) COMP.
            03 EZAHL4JF                       PIC X.
            03 FILLER REDEFINES EZAHL4JF.
               05 EZAHL4JA                       PIC X.
            03 EZAHL4JI                       PIC X(4).
            03 EZAHL5TL                       PIC S9(4) COMP.
            03 EZAHL5TF                       PIC X.
            03 FILLER REDEFINES EZAHL5TF.
               05 EZAHL5TA                       PIC X.
            03 EZAHL5TI                       PIC X(2).
            03 EZAHL5ML                       PIC S9(4) COMP.
            03 EZAHL5MF                       PIC X.
            03 FILLER REDEFINES EZAHL5MF.
               05 EZAHL5MA                       PIC X.
            03 EZAHL5MI                       PIC X(2).
            03 EZAHL5JL                       PIC S9(4) COMP.
            03 EZAHL5JF                       PIC X.
            03 FILLER REDEFINES EZAHL5JF.
               05 EZAHL5JA                       PIC X.
            03 EZAHL5JI                       PIC X(4).
            03 EZAHL1BL                       PIC S9(4) COMP.
            03 EZAHL1BF                       PIC X.
            03 FILLER REDEFINES EZAHL1BF.
               05 EZAHL1BA                       PIC X.
            03 EZAHL1BI                       PIC X(9).
            03 EZAHL2BL                       PIC S9(4) COMP.
            03 EZAHL2BF                       PIC X.
            03 FILLER REDEFINES EZAHL2BF.
               05 EZAHL2BA                       PIC X.
            03 EZAHL2BI                       PIC X(9).
            03 EZAHL3BL                       PIC S9(4) COMP.
            03 EZAHL3BF                       PIC X.
            03 FILLER REDEFINES EZAHL3BF.
               05 EZAHL3BA                       PIC X.
            03 EZAHL3BI                       PIC X(9).
            03 EZAHL4BL                       PIC S9(4) COMP.
            03 EZAHL4BF                       PIC X.
            03 FILLER REDEFINES EZAHL4BF.
               05 EZAHL4BA                       PIC X.
            03 EZAHL4BI                       PIC X(9).
            03 EZAHL5BL                       PIC S9(4) COMP.
            03 EZAHL5BF                       PIC X.
            03 FILLER REDEFINES EZAHL5BF.
               05 EZAHL5BA                       PIC X.
            03 EZAHL5BI                       PIC X(9).

      *  OUTPUT DATA FOR MAP BVB0062
         01 BVB0062O REDEFINES BVB0062I.
            03 FILLER                         PIC X(12).
            03 FILLER                         PIC X(3).
            03 ESTNRO                         PIC X(7).
            03 FILLER                         PIC X(3).
            03 ESTPRZO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 AINFOZ1O                       PIC X(52).
            03 FILLER                         PIC X(3).
            03 AVBNAM1O                       PIC X(40).
            03 FILLER                         PIC X(3).
            03 AVBVOR1O                       PIC X(20).
            03 FILLER                         PIC X(3).
            03 EAUSWO                         PIC X(1).
            03 FILLER                         PIC X(3).
            03 ESTNR2O                        PIC X(7).
            03 FILLER                         PIC X(3).
            03 ESTPZ2O                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 AINFOZ2O                       PIC X(52).
            03 FILLER                         PIC X(3).
            03 AVBNAM2O                       PIC X(40).
            03 FILLER                         PIC X(3).
            03 AVBVOR2O                       PIC X(20).
            03 FILLER                         PIC X(3).
            03 ENFRMO                         PIC X(1).
            03 FILLER                         PIC X(3).
            03 EPROJNRO                       PIC X(10).
            03 FILLER                         PIC X(3).
            03 ENRVONTO                       PIC X(2).
            03 FILLER                         PIC X(3).
            03 ENRVONMO                       PIC X(2).
            03 FILLER                         PIC X(3).
            03 ENRVONJO                       PIC X(4).
            03 FILLER                         PIC X(3).
            03 ENRENDTO                       PIC X(2).
            03 FILLER                         PIC X(3).
            03 ENRENDMO                       PIC X(2).
            03 FILLER                         PIC X(3).
            03 ENRENDJO                       PIC X(4).
            03 FILLER                         PIC X(3).
            03 EMO1O                          PIC X(2).
            03 FILLER                         PIC X(3).
            03 EVONT1O                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 EVONM1O                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 EVONJ1O                        PIC X(4).
            03 FILLER                         PIC X(3).
            03 AVON1O                         PIC X(10).
            03 FILLER                         PIC X(3).
            03 EABBTR1O                       PIC X(9).
            03 FILLER                         PIC X(3).
            03 EMO2O                          PIC X(2).
            03 FILLER                         PIC X(3).
            03 EVONT2O                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 EVONM2O                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 EVONJ2O                        PIC X(4).
            03 FILLER                         PIC X(3).
            03 AVON2O                         PIC X(10).
            03 FILLER                         PIC X(3).
            03 EABBTR2O                       PIC X(9).
            03 FILLER                         PIC X(3).
            03 EMO3O                          PIC X(2).
            03 FILLER                         PIC X(3).
            03 EVONT3O                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 EVONM3O                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 EVONJ3O                        PIC X(4).
            03 FILLER                         PIC X(3).
            03 AVON3O                         PIC X(10).
            03 FILLER                         PIC X(3).
            03 EABBTR3O                       PIC X(9).
            03 FILLER                         PIC X(3).
            03 EMO4O                          PIC X(2).
            03 FILLER                         PIC X(3).
            03 EVONT4O                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 EVONM4O                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 EVONJ4O                        PIC X(4).
            03 FILLER                         PIC X(3).
            03 AVON4O                         PIC X(10).
            03 FILLER                         PIC X(3).
            03 EABBTR4O                       PIC X(9).
            03 FILLER                         PIC X(3).
            03 EMO5O                          PIC X(2).
            03 FILLER                         PIC X(3).
            03 EVONT5O                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 EVONM5O                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 EVONJ5O                        PIC X(4).
            03 FILLER                         PIC X(3).
            03 AVON5O                         PIC X(10).
            03 FILLER                         PIC X(3).
            03 EABBTR5O                       PIC X(9).
            03 FILLER                         PIC X(3).
            03 EMO6O                          PIC X(2).
            03 FILLER                         PIC X(3).
            03 EVONT6O                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 EVONM6O                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 EVONJ6O                        PIC X(4).
            03 FILLER                         PIC X(3).
            03 AVON6O                         PIC X(10).
            03 FILLER                         PIC X(3).
            03 EABBTR6O                       PIC X(9).
            03 FILLER                         PIC X(3).
            03 EZAHL1TO                       PIC X(2).
            03 FILLER                         PIC X(3).
            03 EZAHL1MO                       PIC X(2).
            03 FILLER                         PIC X(3).
            03 EZAHL1JO                       PIC X(4).
            03 FILLER                         PIC X(3).
            03 EZAHL2TO                       PIC X(2).
            03 FILLER                         PIC X(3).
            03 EZAHL2MO                       PIC X(2).
            03 FILLER                         PIC X(3).
            03 EZAHL2JO                       PIC X(4).
            03 FILLER                         PIC X(3).
            03 EZAHL3TO                       PIC X(2).
            03 FILLER                         PIC X(3).
            03 EZAHL3MO                       PIC X(2).
            03 FILLER                         PIC X(3).
            03 EZAHL3JO                       PIC X(4).
            03 FILLER                         PIC X(3).
            03 EZAHL4TO                       PIC X(2).
            03 FILLER                         PIC X(3).
            03 EZAHL4MO                       PIC X(2).
            03 FILLER                         PIC X(3).
            03 EZAHL4JO                       PIC X(4).
            03 FILLER                         PIC X(3).
            03 EZAHL5TO                       PIC X(2).
            03 FILLER                         PIC X(3).
            03 EZAHL5MO                       PIC X(2).
            03 FILLER                         PIC X(3).
            03 EZAHL5JO                       PIC X(4).
            03 FILLER                         PIC X(3).
            03 EZAHL1BO                       PIC X(9).
            03 FILLER                         PIC X(3).
            03 EZAHL2BO                       PIC X(9).
            03 FILLER                         PIC X(3).
            03 EZAHL3BO                       PIC X(9).
            03 FILLER                         PIC X(3).
            03 EZAHL4BO                       PIC X(9).
            03 FILLER                         PIC X(3).
            03 EZAHL5BO                       PIC X(9).


