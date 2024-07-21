      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  30.06.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DBVB0012.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/DBVB0012.CPv  $
      *
      *   Rev 2.1   Jul 01 2003 11:27:04   t13ex22
      *FVB00536: Anpassung des VB33 an neue Seminare
      *
      *   Rev 2.0   Jun 30 2003 15:09:28   t13ex22
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *   Micro Focus BMS Screen Painter (ver MFER25)
      *   MapSet Name   BVB0012
      *   Date Created  06/30/2003
      *   Time Created  15:05:12

      *  Input Data For Map BVB0012
         01 BVB0012I.
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
            03 ANAMEI                         PIC X(30).
            03 AVORNAML                       PIC S9(4) COMP.
            03 AVORNAMF                       PIC X.
            03 FILLER REDEFINES AVORNAMF.
               05 AVORNAMA                       PIC X.
            03 AVORNAMI                       PIC X(30).
            03 AVBSEML                        PIC S9(4) COMP.
            03 AVBSEMF                        PIC X.
            03 FILLER REDEFINES AVBSEMF.
               05 AVBSEMA                        PIC X.
            03 AVBSEMI                        PIC X(1).
            03 EVBSEMTL                       PIC S9(4) COMP.
            03 EVBSEMTF                       PIC X.
            03 FILLER REDEFINES EVBSEMTF.
               05 EVBSEMTA                       PIC X.
            03 EVBSEMTI                       PIC X(2).
            03 EVBSEMML                       PIC S9(4) COMP.
            03 EVBSEMMF                       PIC X.
            03 FILLER REDEFINES EVBSEMMF.
               05 EVBSEMMA                       PIC X.
            03 EVBSEMMI                       PIC X(2).
            03 EVBSEMJL                       PIC S9(4) COMP.
            03 EVBSEMJF                       PIC X.
            03 FILLER REDEFINES EVBSEMJF.
               05 EVBSEMJA                       PIC X.
            03 EVBSEMJI                       PIC X(4).
            03 EALSEML                        PIC S9(4) COMP.
            03 EALSEMF                        PIC X.
            03 FILLER REDEFINES EALSEMF.
               05 EALSEMA                        PIC X.
            03 EALSEMI                        PIC X(1).
            03 EALSEMTL                       PIC S9(4) COMP.
            03 EALSEMTF                       PIC X.
            03 FILLER REDEFINES EALSEMTF.
               05 EALSEMTA                       PIC X.
            03 EALSEMTI                       PIC X(2).
            03 EALSEMML                       PIC S9(4) COMP.
            03 EALSEMMF                       PIC X.
            03 FILLER REDEFINES EALSEMMF.
               05 EALSEMMA                       PIC X.
            03 EALSEMMI                       PIC X(2).
            03 EALSEMJL                       PIC S9(4) COMP.
            03 EALSEMJF                       PIC X.
            03 FILLER REDEFINES EALSEMJF.
               05 EALSEMJA                       PIC X.
            03 EALSEMJI                       PIC X(4).
            03 EALSEMEL                       PIC S9(4) COMP.
            03 EALSEMEF                       PIC X.
            03 FILLER REDEFINES EALSEMEF.
               05 EALSEMEA                       PIC X.
            03 EALSEMEI                       PIC X(1).
            03 AVBPRFL                        PIC S9(4) COMP.
            03 AVBPRFF                        PIC X.
            03 FILLER REDEFINES AVBPRFF.
               05 AVBPRFA                        PIC X.
            03 AVBPRFI                        PIC X(1).
            03 EVBPRFTL                       PIC S9(4) COMP.
            03 EVBPRFTF                       PIC X.
            03 FILLER REDEFINES EVBPRFTF.
               05 EVBPRFTA                       PIC X.
            03 EVBPRFTI                       PIC X(2).
            03 EVBPRFML                       PIC S9(4) COMP.
            03 EVBPRFMF                       PIC X.
            03 FILLER REDEFINES EVBPRFMF.
               05 EVBPRFMA                       PIC X.
            03 EVBPRFMI                       PIC X(2).
            03 EVBPRFJL                       PIC S9(4) COMP.
            03 EVBPRFJF                       PIC X.
            03 FILLER REDEFINES EVBPRFJF.
               05 EVBPRFJA                       PIC X.
            03 EVBPRFJI                       PIC X(4).
            03 EKASEML                        PIC S9(4) COMP.
            03 EKASEMF                        PIC X.
            03 FILLER REDEFINES EKASEMF.
               05 EKASEMA                        PIC X.
            03 EKASEMI                        PIC X(1).
            03 EKASEMTL                       PIC S9(4) COMP.
            03 EKASEMTF                       PIC X.
            03 FILLER REDEFINES EKASEMTF.
               05 EKASEMTA                       PIC X.
            03 EKASEMTI                       PIC X(2).
            03 EKASEMML                       PIC S9(4) COMP.
            03 EKASEMMF                       PIC X.
            03 FILLER REDEFINES EKASEMMF.
               05 EKASEMMA                       PIC X.
            03 EKASEMMI                       PIC X(2).
            03 EKASEMJL                       PIC S9(4) COMP.
            03 EKASEMJF                       PIC X.
            03 FILLER REDEFINES EKASEMJF.
               05 EKASEMJA                       PIC X.
            03 EKASEMJI                       PIC X(4).
            03 EKASEMEL                       PIC S9(4) COMP.
            03 EKASEMEF                       PIC X.
            03 FILLER REDEFINES EKASEMEF.
               05 EKASEMEA                       PIC X.
            03 EKASEMEI                       PIC X(1).
            03 ABDVPRFL                       PIC S9(4) COMP.
            03 ABDVPRFF                       PIC X.
            03 FILLER REDEFINES ABDVPRFF.
               05 ABDVPRFA                       PIC X.
            03 ABDVPRFI                       PIC X(1).
            03 EBDVPRTL                       PIC S9(4) COMP.
            03 EBDVPRTF                       PIC X.
            03 FILLER REDEFINES EBDVPRTF.
               05 EBDVPRTA                       PIC X.
            03 EBDVPRTI                       PIC X(2).
            03 EBDVPRML                       PIC S9(4) COMP.
            03 EBDVPRMF                       PIC X.
            03 FILLER REDEFINES EBDVPRMF.
               05 EBDVPRMA                       PIC X.
            03 EBDVPRMI                       PIC X(2).
            03 EBDVPRJL                       PIC S9(4) COMP.
            03 EBDVPRJF                       PIC X.
            03 FILLER REDEFINES EBDVPRJF.
               05 EBDVPRJA                       PIC X.
            03 EBDVPRJI                       PIC X(4).
            03 ARGSEML                        PIC S9(4) COMP.
            03 ARGSEMF                        PIC X.
            03 FILLER REDEFINES ARGSEMF.
               05 ARGSEMA                        PIC X.
            03 ARGSEMI                        PIC X(1).
            03 ERGSEMTL                       PIC S9(4) COMP.
            03 ERGSEMTF                       PIC X.
            03 FILLER REDEFINES ERGSEMTF.
               05 ERGSEMTA                       PIC X.
            03 ERGSEMTI                       PIC X(2).
            03 ERGSEMML                       PIC S9(4) COMP.
            03 ERGSEMMF                       PIC X.
            03 FILLER REDEFINES ERGSEMMF.
               05 ERGSEMMA                       PIC X.
            03 ERGSEMMI                       PIC X(2).
            03 ERGSEMJL                       PIC S9(4) COMP.
            03 ERGSEMJF                       PIC X.
            03 FILLER REDEFINES ERGSEMJF.
               05 ERGSEMJA                       PIC X.
            03 ERGSEMJI                       PIC X(4).
            03 ERGSEML                        PIC S9(4) COMP.
            03 ERGSEMF                        PIC X.
            03 FILLER REDEFINES ERGSEMF.
               05 ERGSEMA                        PIC X.
            03 ERGSEMI                        PIC X(1).
            03 ABDVMGL                        PIC S9(4) COMP.
            03 ABDVMGF                        PIC X.
            03 FILLER REDEFINES ABDVMGF.
               05 ABDVMGA                        PIC X.
            03 ABDVMGI                        PIC X(1).
            03 EBDVMGL                        PIC S9(4) COMP.
            03 EBDVMGF                        PIC X.
            03 FILLER REDEFINES EBDVMGF.
               05 EBDVMGA                        PIC X.
            03 EBDVMGI                        PIC X(1).
            03 ARAUT1L                        PIC S9(4) COMP.
            03 ARAUT1F                        PIC X.
            03 FILLER REDEFINES ARAUT1F.
               05 ARAUT1A                        PIC X.
            03 ARAUT1I                        PIC X(1).
            03 ERAUT1TL                       PIC S9(4) COMP.
            03 ERAUT1TF                       PIC X.
            03 FILLER REDEFINES ERAUT1TF.
               05 ERAUT1TA                       PIC X.
            03 ERAUT1TI                       PIC X(2).
            03 ERAUT1ML                       PIC S9(4) COMP.
            03 ERAUT1MF                       PIC X.
            03 FILLER REDEFINES ERAUT1MF.
               05 ERAUT1MA                       PIC X.
            03 ERAUT1MI                       PIC X(2).
            03 ERAUT1JL                       PIC S9(4) COMP.
            03 ERAUT1JF                       PIC X.
            03 FILLER REDEFINES ERAUT1JF.
               05 ERAUT1JA                       PIC X.
            03 ERAUT1JI                       PIC X(4).
            03 ADBBVBL                        PIC S9(4) COMP.
            03 ADBBVBF                        PIC X.
            03 FILLER REDEFINES ADBBVBF.
               05 ADBBVBA                        PIC X.
            03 ADBBVBI                        PIC X(1).
            03 ADBBVBTL                       PIC S9(4) COMP.
            03 ADBBVBTF                       PIC X.
            03 FILLER REDEFINES ADBBVBTF.
               05 ADBBVBTA                       PIC X.
            03 ADBBVBTI                       PIC X(2).
            03 ADBBVBML                       PIC S9(4) COMP.
            03 ADBBVBMF                       PIC X.
            03 FILLER REDEFINES ADBBVBMF.
               05 ADBBVBMA                       PIC X.
            03 ADBBVBMI                       PIC X(2).
            03 ADBBVBJL                       PIC S9(4) COMP.
            03 ADBBVBJF                       PIC X.
            03 FILLER REDEFINES ADBBVBJF.
               05 ADBBVBJA                       PIC X.
            03 ADBBVBJI                       PIC X(4).
            03 ARAUT2L                        PIC S9(4) COMP.
            03 ARAUT2F                        PIC X.
            03 FILLER REDEFINES ARAUT2F.
               05 ARAUT2A                        PIC X.
            03 ARAUT2I                        PIC X(1).
            03 ERAUT2TL                       PIC S9(4) COMP.
            03 ERAUT2TF                       PIC X.
            03 FILLER REDEFINES ERAUT2TF.
               05 ERAUT2TA                       PIC X.
            03 ERAUT2TI                       PIC X(2).
            03 ERAUT2ML                       PIC S9(4) COMP.
            03 ERAUT2MF                       PIC X.
            03 FILLER REDEFINES ERAUT2MF.
               05 ERAUT2MA                       PIC X.
            03 ERAUT2MI                       PIC X(2).
            03 ERAUT2JL                       PIC S9(4) COMP.
            03 ERAUT2JF                       PIC X.
            03 FILLER REDEFINES ERAUT2JF.
               05 ERAUT2JA                       PIC X.
            03 ERAUT2JI                       PIC X(4).
            03 ADBBVAL                        PIC S9(4) COMP.
            03 ADBBVAF                        PIC X.
            03 FILLER REDEFINES ADBBVAF.
               05 ADBBVAA                        PIC X.
            03 ADBBVAI                        PIC X(1).
            03 ADBBVATL                       PIC S9(4) COMP.
            03 ADBBVATF                       PIC X.
            03 FILLER REDEFINES ADBBVATF.
               05 ADBBVATA                       PIC X.
            03 ADBBVATI                       PIC X(2).
            03 ADBBVAML                       PIC S9(4) COMP.
            03 ADBBVAMF                       PIC X.
            03 FILLER REDEFINES ADBBVAMF.
               05 ADBBVAMA                       PIC X.
            03 ADBBVAMI                       PIC X(2).
            03 ADBBVAJL                       PIC S9(4) COMP.
            03 ADBBVAJF                       PIC X.
            03 FILLER REDEFINES ADBBVAJF.
               05 ADBBVAJA                       PIC X.
            03 ADBBVAJI                       PIC X(4).
            03 ARAUT3L                        PIC S9(4) COMP.
            03 ARAUT3F                        PIC X.
            03 FILLER REDEFINES ARAUT3F.
               05 ARAUT3A                        PIC X.
            03 ARAUT3I                        PIC X(1).
            03 ERAUT3TL                       PIC S9(4) COMP.
            03 ERAUT3TF                       PIC X.
            03 FILLER REDEFINES ERAUT3TF.
               05 ERAUT3TA                       PIC X.
            03 ERAUT3TI                       PIC X(2).
            03 ERAUT3ML                       PIC S9(4) COMP.
            03 ERAUT3MF                       PIC X.
            03 FILLER REDEFINES ERAUT3MF.
               05 ERAUT3MA                       PIC X.
            03 ERAUT3MI                       PIC X(2).
            03 ERAUT3JL                       PIC S9(4) COMP.
            03 ERAUT3JF                       PIC X.
            03 FILLER REDEFINES ERAUT3JF.
               05 ERAUT3JA                       PIC X.
            03 ERAUT3JI                       PIC X(4).
            03 ARAUT4L                        PIC S9(4) COMP.
            03 ARAUT4F                        PIC X.
            03 FILLER REDEFINES ARAUT4F.
               05 ARAUT4A                        PIC X.
            03 ARAUT4I                        PIC X(1).
            03 ERAUT4TL                       PIC S9(4) COMP.
            03 ERAUT4TF                       PIC X.
            03 FILLER REDEFINES ERAUT4TF.
               05 ERAUT4TA                       PIC X.
            03 ERAUT4TI                       PIC X(2).
            03 ERAUT4ML                       PIC S9(4) COMP.
            03 ERAUT4MF                       PIC X.
            03 FILLER REDEFINES ERAUT4MF.
               05 ERAUT4MA                       PIC X.
            03 ERAUT4MI                       PIC X(2).
            03 ERAUT4JL                       PIC S9(4) COMP.
            03 ERAUT4JF                       PIC X.
            03 FILLER REDEFINES ERAUT4JF.
               05 ERAUT4JA                       PIC X.
            03 ERAUT4JI                       PIC X(4).
            03 AAUFB1TL                       PIC S9(4) COMP.
            03 AAUFB1TF                       PIC X.
            03 FILLER REDEFINES AAUFB1TF.
               05 AAUFB1TA                       PIC X.
            03 AAUFB1TI                       PIC X(15).
            03 AAUFB1L                        PIC S9(4) COMP.
            03 AAUFB1F                        PIC X.
            03 FILLER REDEFINES AAUFB1F.
               05 AAUFB1A                        PIC X.
            03 AAUFB1I                        PIC X(1).
            03 EAUFB1TL                       PIC S9(4) COMP.
            03 EAUFB1TF                       PIC X.
            03 FILLER REDEFINES EAUFB1TF.
               05 EAUFB1TA                       PIC X.
            03 EAUFB1TI                       PIC X(2).
            03 EAUFB1ML                       PIC S9(4) COMP.
            03 EAUFB1MF                       PIC X.
            03 FILLER REDEFINES EAUFB1MF.
               05 EAUFB1MA                       PIC X.
            03 EAUFB1MI                       PIC X(2).
            03 EAUFB1JL                       PIC S9(4) COMP.
            03 EAUFB1JF                       PIC X.
            03 FILLER REDEFINES EAUFB1JF.
               05 EAUFB1JA                       PIC X.
            03 EAUFB1JI                       PIC X(4).
            03 ARAUT5L                        PIC S9(4) COMP.
            03 ARAUT5F                        PIC X.
            03 FILLER REDEFINES ARAUT5F.
               05 ARAUT5A                        PIC X.
            03 ARAUT5I                        PIC X(1).
            03 ERAUT5TL                       PIC S9(4) COMP.
            03 ERAUT5TF                       PIC X.
            03 FILLER REDEFINES ERAUT5TF.
               05 ERAUT5TA                       PIC X.
            03 ERAUT5TI                       PIC X(2).
            03 ERAUT5ML                       PIC S9(4) COMP.
            03 ERAUT5MF                       PIC X.
            03 FILLER REDEFINES ERAUT5MF.
               05 ERAUT5MA                       PIC X.
            03 ERAUT5MI                       PIC X(2).
            03 ERAUT5JL                       PIC S9(4) COMP.
            03 ERAUT5JF                       PIC X.
            03 FILLER REDEFINES ERAUT5JF.
               05 ERAUT5JA                       PIC X.
            03 ERAUT5JI                       PIC X(4).
            03 APRUE1TL                       PIC S9(4) COMP.
            03 APRUE1TF                       PIC X.
            03 FILLER REDEFINES APRUE1TF.
               05 APRUE1TA                       PIC X.
            03 APRUE1TI                       PIC X(15).
            03 APRUEF1L                       PIC S9(4) COMP.
            03 APRUEF1F                       PIC X.
            03 FILLER REDEFINES APRUEF1F.
               05 APRUEF1A                       PIC X.
            03 APRUEF1I                       PIC X(1).
            03 EPRUE1TL                       PIC S9(4) COMP.
            03 EPRUE1TF                       PIC X.
            03 FILLER REDEFINES EPRUE1TF.
               05 EPRUE1TA                       PIC X.
            03 EPRUE1TI                       PIC X(2).
            03 EPRUE1ML                       PIC S9(4) COMP.
            03 EPRUE1MF                       PIC X.
            03 FILLER REDEFINES EPRUE1MF.
               05 EPRUE1MA                       PIC X.
            03 EPRUE1MI                       PIC X(2).
            03 EPRUE1JL                       PIC S9(4) COMP.
            03 EPRUE1JF                       PIC X.
            03 FILLER REDEFINES EPRUE1JF.
               05 EPRUE1JA                       PIC X.
            03 EPRUE1JI                       PIC X(4).
            03 AAUFB2TL                       PIC S9(4) COMP.
            03 AAUFB2TF                       PIC X.
            03 FILLER REDEFINES AAUFB2TF.
               05 AAUFB2TA                       PIC X.
            03 AAUFB2TI                       PIC X(15).
            03 AAUFB2L                        PIC S9(4) COMP.
            03 AAUFB2F                        PIC X.
            03 FILLER REDEFINES AAUFB2F.
               05 AAUFB2A                        PIC X.
            03 AAUFB2I                        PIC X(1).
            03 EAUFB2TL                       PIC S9(4) COMP.
            03 EAUFB2TF                       PIC X.
            03 FILLER REDEFINES EAUFB2TF.
               05 EAUFB2TA                       PIC X.
            03 EAUFB2TI                       PIC X(2).
            03 EAUFB2ML                       PIC S9(4) COMP.
            03 EAUFB2MF                       PIC X.
            03 FILLER REDEFINES EAUFB2MF.
               05 EAUFB2MA                       PIC X.
            03 EAUFB2MI                       PIC X(2).
            03 EAUFB2JL                       PIC S9(4) COMP.
            03 EAUFB2JF                       PIC X.
            03 FILLER REDEFINES EAUFB2JF.
               05 EAUFB2JA                       PIC X.
            03 EAUFB2JI                       PIC X(4).
            03 ACORREL                        PIC S9(4) COMP.
            03 ACORREF                        PIC X.
            03 FILLER REDEFINES ACORREF.
               05 ACORREA                        PIC X.
            03 ACORREI                        PIC X(1).
            03 ECORRETL                       PIC S9(4) COMP.
            03 ECORRETF                       PIC X.
            03 FILLER REDEFINES ECORRETF.
               05 ECORRETA                       PIC X.
            03 ECORRETI                       PIC X(2).
            03 ECORREML                       PIC S9(4) COMP.
            03 ECORREMF                       PIC X.
            03 FILLER REDEFINES ECORREMF.
               05 ECORREMA                       PIC X.
            03 ECORREMI                       PIC X(2).
            03 ECORREJL                       PIC S9(4) COMP.
            03 ECORREJF                       PIC X.
            03 FILLER REDEFINES ECORREJF.
               05 ECORREJA                       PIC X.
            03 ECORREJI                       PIC X(4).
            03 APRUE2TL                       PIC S9(4) COMP.
            03 APRUE2TF                       PIC X.
            03 FILLER REDEFINES APRUE2TF.
               05 APRUE2TA                       PIC X.
            03 APRUE2TI                       PIC X(15).
            03 APRUEF2L                       PIC S9(4) COMP.
            03 APRUEF2F                       PIC X.
            03 FILLER REDEFINES APRUEF2F.
               05 APRUEF2A                       PIC X.
            03 APRUEF2I                       PIC X(1).
            03 EPRUE2TL                       PIC S9(4) COMP.
            03 EPRUE2TF                       PIC X.
            03 FILLER REDEFINES EPRUE2TF.
               05 EPRUE2TA                       PIC X.
            03 EPRUE2TI                       PIC X(2).
            03 EPRUE2ML                       PIC S9(4) COMP.
            03 EPRUE2MF                       PIC X.
            03 FILLER REDEFINES EPRUE2MF.
               05 EPRUE2MA                       PIC X.
            03 EPRUE2MI                       PIC X(2).
            03 EPRUE2JL                       PIC S9(4) COMP.
            03 EPRUE2JF                       PIC X.
            03 FILLER REDEFINES EPRUE2JF.
               05 EPRUE2JA                       PIC X.
            03 EPRUE2JI                       PIC X(4).
            03 AGAPRFL                        PIC S9(4) COMP.
            03 AGAPRFF                        PIC X.
            03 FILLER REDEFINES AGAPRFF.
               05 AGAPRFA                        PIC X.
            03 AGAPRFI                        PIC X(1).
            03 EGAPRFTL                       PIC S9(4) COMP.
            03 EGAPRFTF                       PIC X.
            03 FILLER REDEFINES EGAPRFTF.
               05 EGAPRFTA                       PIC X.
            03 EGAPRFTI                       PIC X(2).
            03 EGAPRFML                       PIC S9(4) COMP.
            03 EGAPRFMF                       PIC X.
            03 FILLER REDEFINES EGAPRFMF.
               05 EGAPRFMA                       PIC X.
            03 EGAPRFMI                       PIC X(2).
            03 EGAPRFJL                       PIC S9(4) COMP.
            03 EGAPRFJF                       PIC X.
            03 FILLER REDEFINES EGAPRFJF.
               05 EGAPRFJA                       PIC X.
            03 EGAPRFJI                       PIC X(4).
            03 AGASTUDL                       PIC S9(4) COMP.
            03 AGASTUDF                       PIC X.
            03 FILLER REDEFINES AGASTUDF.
               05 AGASTUDA                       PIC X.
            03 AGASTUDI                       PIC X(1).
            03 EGASTUTL                       PIC S9(4) COMP.
            03 EGASTUTF                       PIC X.
            03 FILLER REDEFINES EGASTUTF.
               05 EGASTUTA                       PIC X.
            03 EGASTUTI                       PIC X(2).
            03 EGASTUML                       PIC S9(4) COMP.
            03 EGASTUMF                       PIC X.
            03 FILLER REDEFINES EGASTUMF.
               05 EGASTUMA                       PIC X.
            03 EGASTUMI                       PIC X(2).
            03 EGASTUJL                       PIC S9(4) COMP.
            03 EGASTUJF                       PIC X.
            03 FILLER REDEFINES EGASTUJF.
               05 EGASTUJA                       PIC X.
            03 EGASTUJI                       PIC X(4).
            03 AIHKPRFL                       PIC S9(4) COMP.
            03 AIHKPRFF                       PIC X.
            03 FILLER REDEFINES AIHKPRFF.
               05 AIHKPRFA                       PIC X.
            03 AIHKPRFI                       PIC X(1).
            03 EIHKPRTL                       PIC S9(4) COMP.
            03 EIHKPRTF                       PIC X.
            03 FILLER REDEFINES EIHKPRTF.
               05 EIHKPRTA                       PIC X.
            03 EIHKPRTI                       PIC X(2).
            03 EIHKPRML                       PIC S9(4) COMP.
            03 EIHKPRMF                       PIC X.
            03 FILLER REDEFINES EIHKPRMF.
               05 EIHKPRMA                       PIC X.
            03 EIHKPRMI                       PIC X(2).
            03 EIHKPRJL                       PIC S9(4) COMP.
            03 EIHKPRJF                       PIC X.
            03 FILLER REDEFINES EIHKPRJF.
               05 EIHKPRJA                       PIC X.
            03 EIHKPRJI                       PIC X(4).
            03 ADAVL                          PIC S9(4) COMP.
            03 ADAVF                          PIC X.
            03 FILLER REDEFINES ADAVF.
               05 ADAVA                          PIC X.
            03 ADAVI                          PIC X(1).
            03 EDAVTL                         PIC S9(4) COMP.
            03 EDAVTF                         PIC X.
            03 FILLER REDEFINES EDAVTF.
               05 EDAVTA                         PIC X.
            03 EDAVTI                         PIC X(2).
            03 EDAVML                         PIC S9(4) COMP.
            03 EDAVMF                         PIC X.
            03 FILLER REDEFINES EDAVMF.
               05 EDAVMA                         PIC X.
            03 EDAVMI                         PIC X(2).
            03 EDAVJL                         PIC S9(4) COMP.
            03 EDAVJF                         PIC X.
            03 FILLER REDEFINES EDAVJF.
               05 EDAVJA                         PIC X.
            03 EDAVJI                         PIC X(4).

      *  Output Data For Map BVB0012
         01 BVB0012O REDEFINES BVB0012I.
            03 FILLER                         PIC X(12).
            03 FILLER                         PIC X(3).
            03 ESTNRO                         PIC X(7).
            03 FILLER                         PIC X(3).
            03 ESTPRZO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 AINFOZLO                       PIC X(65).
            03 FILLER                         PIC X(3).
            03 ANAMEO                         PIC X(30).
            03 FILLER                         PIC X(3).
            03 AVORNAMO                       PIC X(30).
            03 FILLER                         PIC X(3).
            03 AVBSEMO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 EVBSEMTO                       PIC X(2).
            03 FILLER                         PIC X(3).
            03 EVBSEMMO                       PIC X(2).
            03 FILLER                         PIC X(3).
            03 EVBSEMJO                       PIC X(4).
            03 FILLER                         PIC X(3).
            03 EALSEMO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 EALSEMTO                       PIC X(2).
            03 FILLER                         PIC X(3).
            03 EALSEMMO                       PIC X(2).
            03 FILLER                         PIC X(3).
            03 EALSEMJO                       PIC X(4).
            03 FILLER                         PIC X(3).
            03 EALSEMEO                       PIC X(1).
            03 FILLER                         PIC X(3).
            03 AVBPRFO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 EVBPRFTO                       PIC X(2).
            03 FILLER                         PIC X(3).
            03 EVBPRFMO                       PIC X(2).
            03 FILLER                         PIC X(3).
            03 EVBPRFJO                       PIC X(4).
            03 FILLER                         PIC X(3).
            03 EKASEMO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 EKASEMTO                       PIC X(2).
            03 FILLER                         PIC X(3).
            03 EKASEMMO                       PIC X(2).
            03 FILLER                         PIC X(3).
            03 EKASEMJO                       PIC X(4).
            03 FILLER                         PIC X(3).
            03 EKASEMEO                       PIC X(1).
            03 FILLER                         PIC X(3).
            03 ABDVPRFO                       PIC X(1).
            03 FILLER                         PIC X(3).
            03 EBDVPRTO                       PIC X(2).
            03 FILLER                         PIC X(3).
            03 EBDVPRMO                       PIC X(2).
            03 FILLER                         PIC X(3).
            03 EBDVPRJO                       PIC X(4).
            03 FILLER                         PIC X(3).
            03 ARGSEMO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 ERGSEMTO                       PIC X(2).
            03 FILLER                         PIC X(3).
            03 ERGSEMMO                       PIC X(2).
            03 FILLER                         PIC X(3).
            03 ERGSEMJO                       PIC X(4).
            03 FILLER                         PIC X(3).
            03 ERGSEMO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 ABDVMGO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 EBDVMGO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 ARAUT1O                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 ERAUT1TO                       PIC X(2).
            03 FILLER                         PIC X(3).
            03 ERAUT1MO                       PIC X(2).
            03 FILLER                         PIC X(3).
            03 ERAUT1JO                       PIC X(4).
            03 FILLER                         PIC X(3).
            03 ADBBVBO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 ADBBVBTO                       PIC X(2).
            03 FILLER                         PIC X(3).
            03 ADBBVBMO                       PIC X(2).
            03 FILLER                         PIC X(3).
            03 ADBBVBJO                       PIC X(4).
            03 FILLER                         PIC X(3).
            03 ARAUT2O                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 ERAUT2TO                       PIC X(2).
            03 FILLER                         PIC X(3).
            03 ERAUT2MO                       PIC X(2).
            03 FILLER                         PIC X(3).
            03 ERAUT2JO                       PIC X(4).
            03 FILLER                         PIC X(3).
            03 ADBBVAO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 ADBBVATO                       PIC X(2).
            03 FILLER                         PIC X(3).
            03 ADBBVAMO                       PIC X(2).
            03 FILLER                         PIC X(3).
            03 ADBBVAJO                       PIC X(4).
            03 FILLER                         PIC X(3).
            03 ARAUT3O                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 ERAUT3TO                       PIC X(2).
            03 FILLER                         PIC X(3).
            03 ERAUT3MO                       PIC X(2).
            03 FILLER                         PIC X(3).
            03 ERAUT3JO                       PIC X(4).
            03 FILLER                         PIC X(3).
            03 ARAUT4O                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 ERAUT4TO                       PIC X(2).
            03 FILLER                         PIC X(3).
            03 ERAUT4MO                       PIC X(2).
            03 FILLER                         PIC X(3).
            03 ERAUT4JO                       PIC X(4).
            03 FILLER                         PIC X(3).
            03 AAUFB1TO                       PIC X(15).
            03 FILLER                         PIC X(3).
            03 AAUFB1O                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 EAUFB1TO                       PIC X(2).
            03 FILLER                         PIC X(3).
            03 EAUFB1MO                       PIC X(2).
            03 FILLER                         PIC X(3).
            03 EAUFB1JO                       PIC X(4).
            03 FILLER                         PIC X(3).
            03 ARAUT5O                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 ERAUT5TO                       PIC X(2).
            03 FILLER                         PIC X(3).
            03 ERAUT5MO                       PIC X(2).
            03 FILLER                         PIC X(3).
            03 ERAUT5JO                       PIC X(4).
            03 FILLER                         PIC X(3).
            03 APRUE1TO                       PIC X(15).
            03 FILLER                         PIC X(3).
            03 APRUEF1O                       PIC X(1).
            03 FILLER                         PIC X(3).
            03 EPRUE1TO                       PIC X(2).
            03 FILLER                         PIC X(3).
            03 EPRUE1MO                       PIC X(2).
            03 FILLER                         PIC X(3).
            03 EPRUE1JO                       PIC X(4).
            03 FILLER                         PIC X(3).
            03 AAUFB2TO                       PIC X(15).
            03 FILLER                         PIC X(3).
            03 AAUFB2O                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 EAUFB2TO                       PIC X(2).
            03 FILLER                         PIC X(3).
            03 EAUFB2MO                       PIC X(2).
            03 FILLER                         PIC X(3).
            03 EAUFB2JO                       PIC X(4).
            03 FILLER                         PIC X(3).
            03 ACORREO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 ECORRETO                       PIC X(2).
            03 FILLER                         PIC X(3).
            03 ECORREMO                       PIC X(2).
            03 FILLER                         PIC X(3).
            03 ECORREJO                       PIC X(4).
            03 FILLER                         PIC X(3).
            03 APRUE2TO                       PIC X(15).
            03 FILLER                         PIC X(3).
            03 APRUEF2O                       PIC X(1).
            03 FILLER                         PIC X(3).
            03 EPRUE2TO                       PIC X(2).
            03 FILLER                         PIC X(3).
            03 EPRUE2MO                       PIC X(2).
            03 FILLER                         PIC X(3).
            03 EPRUE2JO                       PIC X(4).
            03 FILLER                         PIC X(3).
            03 AGAPRFO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 EGAPRFTO                       PIC X(2).
            03 FILLER                         PIC X(3).
            03 EGAPRFMO                       PIC X(2).
            03 FILLER                         PIC X(3).
            03 EGAPRFJO                       PIC X(4).
            03 FILLER                         PIC X(3).
            03 AGASTUDO                       PIC X(1).
            03 FILLER                         PIC X(3).
            03 EGASTUTO                       PIC X(2).
            03 FILLER                         PIC X(3).
            03 EGASTUMO                       PIC X(2).
            03 FILLER                         PIC X(3).
            03 EGASTUJO                       PIC X(4).
            03 FILLER                         PIC X(3).
            03 AIHKPRFO                       PIC X(1).
            03 FILLER                         PIC X(3).
            03 EIHKPRTO                       PIC X(2).
            03 FILLER                         PIC X(3).
            03 EIHKPRMO                       PIC X(2).
            03 FILLER                         PIC X(3).
            03 EIHKPRJO                       PIC X(4).
            03 FILLER                         PIC X(3).
            03 ADAVO                          PIC X(1).
            03 FILLER                         PIC X(3).
            03 EDAVTO                         PIC X(2).
            03 FILLER                         PIC X(3).
            03 EDAVMO                         PIC X(2).
            03 FILLER                         PIC X(3).
            03 EDAVJO                         PIC X(4).
