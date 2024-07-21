      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  03.06.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DBSL0017.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DBSL0017.CPv  $
      *
      *   Rev 2.2   19 Nov 2019 08:32:36   t13ae56
      *ASL00584: SL15 - Pflege Tabelle VHV
      *
      *   Rev 2.1   Aug 27 2019 07:53:42   t13ae56
      *ASL00584: SL15 - Pflege Tabelle VHV
      *
      *   Rev 2.0   Jun 03 2019 15:14:34   t13ae56
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *   MICRO FOCUS ENTERPRISE DEVELOPER FOR ECLIPSE  4.0.00243
      *   MICRO FOCUS BMS SCREEN PAINTER (VER BMSIDE_4-0-05)
      *   MAPSET NAME   BSL0017
      *   DATE CREATED  11/12/2019
      *   TIME CREATED  08:52:00

      *  INPUT DATA FOR MAP BSL0017
         01 BSL0017I.
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
            03 ABDVL                          PIC S9(4) COMP.
            03 ABDVF                          PIC X.
            03 FILLER REDEFINES ABDVF.
               05 ABDVA                          PIC X.
            03 ABDVI                          PIC X(1).
            03 ASTFABL                        PIC S9(4) COMP.
            03 ASTFABF                        PIC X.
            03 FILLER REDEFINES ASTFABF.
               05 ASTFABA                        PIC X.
            03 ASTFABI                        PIC X(10).
            03 ATEXT1L                        PIC S9(4) COMP.
            03 ATEXT1F                        PIC X.
            03 FILLER REDEFINES ATEXT1F.
               05 ATEXT1A                        PIC X.
            03 ATEXT1I                        PIC X(30).
            03 EANZ1L                         PIC S9(4) COMP.
            03 EANZ1F                         PIC X.
            03 FILLER REDEFINES EANZ1F.
               05 EANZ1A                         PIC X.
            03 EANZ1I                         PIC X(1).
            03 EBEG1TL                        PIC S9(4) COMP.
            03 EBEG1TF                        PIC X.
            03 FILLER REDEFINES EBEG1TF.
               05 EBEG1TA                        PIC X.
            03 EBEG1TI                        PIC X(2).
            03 EBEG1ML                        PIC S9(4) COMP.
            03 EBEG1MF                        PIC X.
            03 FILLER REDEFINES EBEG1MF.
               05 EBEG1MA                        PIC X.
            03 EBEG1MI                        PIC X(2).
            03 EBEG1JL                        PIC S9(4) COMP.
            03 EBEG1JF                        PIC X.
            03 FILLER REDEFINES EBEG1JF.
               05 EBEG1JA                        PIC X.
            03 EBEG1JI                        PIC X(4).
            03 EEND1TL                        PIC S9(4) COMP.
            03 EEND1TF                        PIC X.
            03 FILLER REDEFINES EEND1TF.
               05 EEND1TA                        PIC X.
            03 EEND1TI                        PIC X(2).
            03 EEND1ML                        PIC S9(4) COMP.
            03 EEND1MF                        PIC X.
            03 FILLER REDEFINES EEND1MF.
               05 EEND1MA                        PIC X.
            03 EEND1MI                        PIC X(2).
            03 EEND1JL                        PIC S9(4) COMP.
            03 EEND1JF                        PIC X.
            03 FILLER REDEFINES EEND1JF.
               05 EEND1JA                        PIC X.
            03 EEND1JI                        PIC X(4).
            03 ABETR1L                        PIC S9(4) COMP.
            03 ABETR1F                        PIC X.
            03 FILLER REDEFINES ABETR1F.
               05 ABETR1A                        PIC X.
            03 ABETR1I                        PIC X(7).
            03 AVSNR1L                        PIC S9(4) COMP.
            03 AVSNR1F                        PIC X.
            03 FILLER REDEFINES AVSNR1F.
               05 AVSNR1A                        PIC X.
            03 AVSNR1I                        PIC X(9).
            03 ATEXT2L                        PIC S9(4) COMP.
            03 ATEXT2F                        PIC X.
            03 FILLER REDEFINES ATEXT2F.
               05 ATEXT2A                        PIC X.
            03 ATEXT2I                        PIC X(30).
            03 EANZ2L                         PIC S9(4) COMP.
            03 EANZ2F                         PIC X.
            03 FILLER REDEFINES EANZ2F.
               05 EANZ2A                         PIC X.
            03 EANZ2I                         PIC X(1).
            03 EBEG2TL                        PIC S9(4) COMP.
            03 EBEG2TF                        PIC X.
            03 FILLER REDEFINES EBEG2TF.
               05 EBEG2TA                        PIC X.
            03 EBEG2TI                        PIC X(2).
            03 EBEG2ML                        PIC S9(4) COMP.
            03 EBEG2MF                        PIC X.
            03 FILLER REDEFINES EBEG2MF.
               05 EBEG2MA                        PIC X.
            03 EBEG2MI                        PIC X(2).
            03 EBEG2JL                        PIC S9(4) COMP.
            03 EBEG2JF                        PIC X.
            03 FILLER REDEFINES EBEG2JF.
               05 EBEG2JA                        PIC X.
            03 EBEG2JI                        PIC X(4).
            03 EEND2TL                        PIC S9(4) COMP.
            03 EEND2TF                        PIC X.
            03 FILLER REDEFINES EEND2TF.
               05 EEND2TA                        PIC X.
            03 EEND2TI                        PIC X(2).
            03 EEND2ML                        PIC S9(4) COMP.
            03 EEND2MF                        PIC X.
            03 FILLER REDEFINES EEND2MF.
               05 EEND2MA                        PIC X.
            03 EEND2MI                        PIC X(2).
            03 EEND2JL                        PIC S9(4) COMP.
            03 EEND2JF                        PIC X.
            03 FILLER REDEFINES EEND2JF.
               05 EEND2JA                        PIC X.
            03 EEND2JI                        PIC X(4).
            03 ABETR2L                        PIC S9(4) COMP.
            03 ABETR2F                        PIC X.
            03 FILLER REDEFINES ABETR2F.
               05 ABETR2A                        PIC X.
            03 ABETR2I                        PIC X(7).
            03 AVSNR2L                        PIC S9(4) COMP.
            03 AVSNR2F                        PIC X.
            03 FILLER REDEFINES AVSNR2F.
               05 AVSNR2A                        PIC X.
            03 AVSNR2I                        PIC X(9).
            03 ATEXT3L                        PIC S9(4) COMP.
            03 ATEXT3F                        PIC X.
            03 FILLER REDEFINES ATEXT3F.
               05 ATEXT3A                        PIC X.
            03 ATEXT3I                        PIC X(30).
            03 EANZ3L                         PIC S9(4) COMP.
            03 EANZ3F                         PIC X.
            03 FILLER REDEFINES EANZ3F.
               05 EANZ3A                         PIC X.
            03 EANZ3I                         PIC X(1).
            03 EBEG3TL                        PIC S9(4) COMP.
            03 EBEG3TF                        PIC X.
            03 FILLER REDEFINES EBEG3TF.
               05 EBEG3TA                        PIC X.
            03 EBEG3TI                        PIC X(2).
            03 EBEG3ML                        PIC S9(4) COMP.
            03 EBEG3MF                        PIC X.
            03 FILLER REDEFINES EBEG3MF.
               05 EBEG3MA                        PIC X.
            03 EBEG3MI                        PIC X(2).
            03 EBEG3JL                        PIC S9(4) COMP.
            03 EBEG3JF                        PIC X.
            03 FILLER REDEFINES EBEG3JF.
               05 EBEG3JA                        PIC X.
            03 EBEG3JI                        PIC X(4).
            03 EEND3TL                        PIC S9(4) COMP.
            03 EEND3TF                        PIC X.
            03 FILLER REDEFINES EEND3TF.
               05 EEND3TA                        PIC X.
            03 EEND3TI                        PIC X(2).
            03 EEND3ML                        PIC S9(4) COMP.
            03 EEND3MF                        PIC X.
            03 FILLER REDEFINES EEND3MF.
               05 EEND3MA                        PIC X.
            03 EEND3MI                        PIC X(2).
            03 EEND3JL                        PIC S9(4) COMP.
            03 EEND3JF                        PIC X.
            03 FILLER REDEFINES EEND3JF.
               05 EEND3JA                        PIC X.
            03 EEND3JI                        PIC X(4).
            03 ABETR3L                        PIC S9(4) COMP.
            03 ABETR3F                        PIC X.
            03 FILLER REDEFINES ABETR3F.
               05 ABETR3A                        PIC X.
            03 ABETR3I                        PIC X(7).
            03 AVSNR3L                        PIC S9(4) COMP.
            03 AVSNR3F                        PIC X.
            03 FILLER REDEFINES AVSNR3F.
               05 AVSNR3A                        PIC X.
            03 AVSNR3I                        PIC X(9).
            03 ATEXT4L                        PIC S9(4) COMP.
            03 ATEXT4F                        PIC X.
            03 FILLER REDEFINES ATEXT4F.
               05 ATEXT4A                        PIC X.
            03 ATEXT4I                        PIC X(30).
            03 EANZ4L                         PIC S9(4) COMP.
            03 EANZ4F                         PIC X.
            03 FILLER REDEFINES EANZ4F.
               05 EANZ4A                         PIC X.
            03 EANZ4I                         PIC X(1).
            03 EBEG4TL                        PIC S9(4) COMP.
            03 EBEG4TF                        PIC X.
            03 FILLER REDEFINES EBEG4TF.
               05 EBEG4TA                        PIC X.
            03 EBEG4TI                        PIC X(2).
            03 EBEG4ML                        PIC S9(4) COMP.
            03 EBEG4MF                        PIC X.
            03 FILLER REDEFINES EBEG4MF.
               05 EBEG4MA                        PIC X.
            03 EBEG4MI                        PIC X(2).
            03 EBEG4JL                        PIC S9(4) COMP.
            03 EBEG4JF                        PIC X.
            03 FILLER REDEFINES EBEG4JF.
               05 EBEG4JA                        PIC X.
            03 EBEG4JI                        PIC X(4).
            03 EEND4TL                        PIC S9(4) COMP.
            03 EEND4TF                        PIC X.
            03 FILLER REDEFINES EEND4TF.
               05 EEND4TA                        PIC X.
            03 EEND4TI                        PIC X(2).
            03 EEND4ML                        PIC S9(4) COMP.
            03 EEND4MF                        PIC X.
            03 FILLER REDEFINES EEND4MF.
               05 EEND4MA                        PIC X.
            03 EEND4MI                        PIC X(2).
            03 EEND4JL                        PIC S9(4) COMP.
            03 EEND4JF                        PIC X.
            03 FILLER REDEFINES EEND4JF.
               05 EEND4JA                        PIC X.
            03 EEND4JI                        PIC X(4).
            03 ABETR4L                        PIC S9(4) COMP.
            03 ABETR4F                        PIC X.
            03 FILLER REDEFINES ABETR4F.
               05 ABETR4A                        PIC X.
            03 ABETR4I                        PIC X(7).
            03 AVSNR4L                        PIC S9(4) COMP.
            03 AVSNR4F                        PIC X.
            03 FILLER REDEFINES AVSNR4F.
               05 AVSNR4A                        PIC X.
            03 AVSNR4I                        PIC X(9).
            03 ATEXT5L                        PIC S9(4) COMP.
            03 ATEXT5F                        PIC X.
            03 FILLER REDEFINES ATEXT5F.
               05 ATEXT5A                        PIC X.
            03 ATEXT5I                        PIC X(30).
            03 EANZ5L                         PIC S9(4) COMP.
            03 EANZ5F                         PIC X.
            03 FILLER REDEFINES EANZ5F.
               05 EANZ5A                         PIC X.
            03 EANZ5I                         PIC X(1).
            03 EBEG5TL                        PIC S9(4) COMP.
            03 EBEG5TF                        PIC X.
            03 FILLER REDEFINES EBEG5TF.
               05 EBEG5TA                        PIC X.
            03 EBEG5TI                        PIC X(2).
            03 EBEG5ML                        PIC S9(4) COMP.
            03 EBEG5MF                        PIC X.
            03 FILLER REDEFINES EBEG5MF.
               05 EBEG5MA                        PIC X.
            03 EBEG5MI                        PIC X(2).
            03 EBEG5JL                        PIC S9(4) COMP.
            03 EBEG5JF                        PIC X.
            03 FILLER REDEFINES EBEG5JF.
               05 EBEG5JA                        PIC X.
            03 EBEG5JI                        PIC X(4).
            03 EEND5TL                        PIC S9(4) COMP.
            03 EEND5TF                        PIC X.
            03 FILLER REDEFINES EEND5TF.
               05 EEND5TA                        PIC X.
            03 EEND5TI                        PIC X(2).
            03 EEND5ML                        PIC S9(4) COMP.
            03 EEND5MF                        PIC X.
            03 FILLER REDEFINES EEND5MF.
               05 EEND5MA                        PIC X.
            03 EEND5MI                        PIC X(2).
            03 EEND5JL                        PIC S9(4) COMP.
            03 EEND5JF                        PIC X.
            03 FILLER REDEFINES EEND5JF.
               05 EEND5JA                        PIC X.
            03 EEND5JI                        PIC X(4).
            03 ABETR5L                        PIC S9(4) COMP.
            03 ABETR5F                        PIC X.
            03 FILLER REDEFINES ABETR5F.
               05 ABETR5A                        PIC X.
            03 ABETR5I                        PIC X(7).
            03 AVSNR5L                        PIC S9(4) COMP.
            03 AVSNR5F                        PIC X.
            03 FILLER REDEFINES AVSNR5F.
               05 AVSNR5A                        PIC X.
            03 AVSNR5I                        PIC X(9).
            03 ATEXT6L                        PIC S9(4) COMP.
            03 ATEXT6F                        PIC X.
            03 FILLER REDEFINES ATEXT6F.
               05 ATEXT6A                        PIC X.
            03 ATEXT6I                        PIC X(30).
            03 EANZ6L                         PIC S9(4) COMP.
            03 EANZ6F                         PIC X.
            03 FILLER REDEFINES EANZ6F.
               05 EANZ6A                         PIC X.
            03 EANZ6I                         PIC X(1).
            03 EBEG6TL                        PIC S9(4) COMP.
            03 EBEG6TF                        PIC X.
            03 FILLER REDEFINES EBEG6TF.
               05 EBEG6TA                        PIC X.
            03 EBEG6TI                        PIC X(2).
            03 EBEG6ML                        PIC S9(4) COMP.
            03 EBEG6MF                        PIC X.
            03 FILLER REDEFINES EBEG6MF.
               05 EBEG6MA                        PIC X.
            03 EBEG6MI                        PIC X(2).
            03 EBEG6JL                        PIC S9(4) COMP.
            03 EBEG6JF                        PIC X.
            03 FILLER REDEFINES EBEG6JF.
               05 EBEG6JA                        PIC X.
            03 EBEG6JI                        PIC X(4).
            03 EEND6TL                        PIC S9(4) COMP.
            03 EEND6TF                        PIC X.
            03 FILLER REDEFINES EEND6TF.
               05 EEND6TA                        PIC X.
            03 EEND6TI                        PIC X(2).
            03 EEND6ML                        PIC S9(4) COMP.
            03 EEND6MF                        PIC X.
            03 FILLER REDEFINES EEND6MF.
               05 EEND6MA                        PIC X.
            03 EEND6MI                        PIC X(2).
            03 EEND6JL                        PIC S9(4) COMP.
            03 EEND6JF                        PIC X.
            03 FILLER REDEFINES EEND6JF.
               05 EEND6JA                        PIC X.
            03 EEND6JI                        PIC X(4).
            03 ABETR6L                        PIC S9(4) COMP.
            03 ABETR6F                        PIC X.
            03 FILLER REDEFINES ABETR6F.
               05 ABETR6A                        PIC X.
            03 ABETR6I                        PIC X(7).
            03 AVSNR6L                        PIC S9(4) COMP.
            03 AVSNR6F                        PIC X.
            03 FILLER REDEFINES AVSNR6F.
               05 AVSNR6A                        PIC X.
            03 AVSNR6I                        PIC X(9).
            03 EEINVTL                        PIC S9(4) COMP.
            03 EEINVTF                        PIC X.
            03 FILLER REDEFINES EEINVTF.
               05 EEINVTA                        PIC X.
            03 EEINVTI                        PIC X(50).
            03 EINFO1L                        PIC S9(4) COMP.
            03 EINFO1F                        PIC X.
            03 FILLER REDEFINES EINFO1F.
               05 EINFO1A                        PIC X.
            03 EINFO1I                        PIC X(50).
            03 EINFO2L                        PIC S9(4) COMP.
            03 EINFO2F                        PIC X.
            03 FILLER REDEFINES EINFO2F.
               05 EINFO2A                        PIC X.
            03 EINFO2I                        PIC X(50).
            03 EBETREL                        PIC S9(4) COMP.
            03 EBETREF                        PIC X.
            03 FILLER REDEFINES EBETREF.
               05 EBETREA                        PIC X.
            03 EBETREI                        PIC X(7).
            03 EERSTTL                        PIC S9(4) COMP.
            03 EERSTTF                        PIC X.
            03 FILLER REDEFINES EERSTTF.
               05 EERSTTA                        PIC X.
            03 EERSTTI                        PIC X(2).
            03 EERSTML                        PIC S9(4) COMP.
            03 EERSTMF                        PIC X.
            03 FILLER REDEFINES EERSTMF.
               05 EERSTMA                        PIC X.
            03 EERSTMI                        PIC X(2).
            03 EERSTJL                        PIC S9(4) COMP.
            03 EERSTJF                        PIC X.
            03 FILLER REDEFINES EERSTJF.
               05 EERSTJA                        PIC X.
            03 EERSTJI                        PIC X(4).

      *  OUTPUT DATA FOR MAP BSL0017
         01 BSL0017O REDEFINES BSL0017I.
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
            03 ABDVO                          PIC X(1).
            03 FILLER                         PIC X(3).
            03 ASTFABO                        PIC X(10).
            03 FILLER                         PIC X(3).
            03 ATEXT1O                        PIC X(30).
            03 FILLER                         PIC X(3).
            03 EANZ1O                         PIC X(1).
            03 FILLER                         PIC X(3).
            03 EBEG1TO                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 EBEG1MO                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 EBEG1JO                        PIC X(4).
            03 FILLER                         PIC X(3).
            03 EEND1TO                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 EEND1MO                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 EEND1JO                        PIC X(4).
            03 FILLER                         PIC X(3).
            03 ABETR1O                        PIC X(7).
            03 FILLER                         PIC X(3).
            03 AVSNR1O                        PIC X(9).
            03 FILLER                         PIC X(3).
            03 ATEXT2O                        PIC X(30).
            03 FILLER                         PIC X(3).
            03 EANZ2O                         PIC X(1).
            03 FILLER                         PIC X(3).
            03 EBEG2TO                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 EBEG2MO                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 EBEG2JO                        PIC X(4).
            03 FILLER                         PIC X(3).
            03 EEND2TO                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 EEND2MO                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 EEND2JO                        PIC X(4).
            03 FILLER                         PIC X(3).
            03 ABETR2O                        PIC X(7).
            03 FILLER                         PIC X(3).
            03 AVSNR2O                        PIC X(9).
            03 FILLER                         PIC X(3).
            03 ATEXT3O                        PIC X(30).
            03 FILLER                         PIC X(3).
            03 EANZ3O                         PIC X(1).
            03 FILLER                         PIC X(3).
            03 EBEG3TO                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 EBEG3MO                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 EBEG3JO                        PIC X(4).
            03 FILLER                         PIC X(3).
            03 EEND3TO                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 EEND3MO                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 EEND3JO                        PIC X(4).
            03 FILLER                         PIC X(3).
            03 ABETR3O                        PIC X(7).
            03 FILLER                         PIC X(3).
            03 AVSNR3O                        PIC X(9).
            03 FILLER                         PIC X(3).
            03 ATEXT4O                        PIC X(30).
            03 FILLER                         PIC X(3).
            03 EANZ4O                         PIC X(1).
            03 FILLER                         PIC X(3).
            03 EBEG4TO                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 EBEG4MO                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 EBEG4JO                        PIC X(4).
            03 FILLER                         PIC X(3).
            03 EEND4TO                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 EEND4MO                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 EEND4JO                        PIC X(4).
            03 FILLER                         PIC X(3).
            03 ABETR4O                        PIC X(7).
            03 FILLER                         PIC X(3).
            03 AVSNR4O                        PIC X(9).
            03 FILLER                         PIC X(3).
            03 ATEXT5O                        PIC X(30).
            03 FILLER                         PIC X(3).
            03 EANZ5O                         PIC X(1).
            03 FILLER                         PIC X(3).
            03 EBEG5TO                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 EBEG5MO                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 EBEG5JO                        PIC X(4).
            03 FILLER                         PIC X(3).
            03 EEND5TO                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 EEND5MO                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 EEND5JO                        PIC X(4).
            03 FILLER                         PIC X(3).
            03 ABETR5O                        PIC X(7).
            03 FILLER                         PIC X(3).
            03 AVSNR5O                        PIC X(9).
            03 FILLER                         PIC X(3).
            03 ATEXT6O                        PIC X(30).
            03 FILLER                         PIC X(3).
            03 EANZ6O                         PIC X(1).
            03 FILLER                         PIC X(3).
            03 EBEG6TO                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 EBEG6MO                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 EBEG6JO                        PIC X(4).
            03 FILLER                         PIC X(3).
            03 EEND6TO                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 EEND6MO                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 EEND6JO                        PIC X(4).
            03 FILLER                         PIC X(3).
            03 ABETR6O                        PIC X(7).
            03 FILLER                         PIC X(3).
            03 AVSNR6O                        PIC X(9).
            03 FILLER                         PIC X(3).
            03 EEINVTO                        PIC X(50).
            03 FILLER                         PIC X(3).
            03 EINFO1O                        PIC X(50).
            03 FILLER                         PIC X(3).
            03 EINFO2O                        PIC X(50).
            03 FILLER                         PIC X(3).
            03 EBETREO                        PIC X(7).
            03 FILLER                         PIC X(3).
            03 EERSTTO                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 EERSTMO                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 EERSTJO                        PIC X(4).


