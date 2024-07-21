      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.11.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0175.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRAN0175.CPv  $
      *
      *   Rev 2.3   Mar 30 2007 10:03:24   t13ae34
      *AAN00655: fehlendes copy dran0175
      *
      *   Rev 2.2   01 Dec 2006 14:56:54   t13ae23
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.1   16 Nov 2006 10:34:50   t13ae23
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   14 Nov 2006 10:35:02   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0175  FUNKTION SELECT STANDARD-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: RAHNER                                          *
      *    DATUM:    14.11.2006                                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00655!14.11.06  2.1! NEUERSTELLUNG             ! RAHNER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0175                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  LESEN  <<<        LESEN ANTRAGSDATEI Ü ALTERNATIV    *
      *                                                INDEX         *
      ****************************************************************
       01 PI-DRAN0175.
          10 PI-DRAN0175-UFKT                  PIC  X.
              88 PI-DRAN0175-UFKT-READ-FIRST   VALUE '1'.
              88 PI-DRAN0175-UFKT-READ-NEXT    VALUE '2'.
              88 PI-DRAN0175-UFKT-READ-LAST    VALUE '3'.
          10 PI-DRAN0175-GES                   PIC 9(002).
          10 PI-DRAN0175-VNR                   PIC 9(015).
      *
       01 PO-DRAN0175.
          05 PO-DRAN0175-RC                    PIC  X(003).
             88 PO-DRAN0175-RC-OK              VALUE 'OK '.
             88 PO-DRAN0175-RC-NOT-FOUND       VALUE 'NFD'.

      ***  ENTSPRICHT DEM COPY 'UU020S'
          05 PO-DRAN0175-ANSATZ-NEU.
              10  PO-DRAN0175-ASA                     PIC 99.
              10  PO-DRAN0175-ANKEY.
                  20  PO-DRAN0175-AMANR               PIC 9(7) COMP-3.
                  20  PO-DRAN0175-AGES                PIC 99.
                  20  PO-DRAN0175-AVNR                PIC 9(15) COMP-3.
      *   *
      *   *    BESTANDS-SATZ
      *   *
              10  PO-DRAN0175-ASATZ.
                  20 PO-DRAN0175-APRZMA               PIC X.
                  20  PO-DRAN0175-APRZVG              PIC XX.
      *
                  20  PO-DRAN0175-AMATCHCODE          PIC X(8).
                  20  FILLER  REDEFINES PO-DRAN0175-AMATCHCODE.
                      30  PO-DRAN0175-ACODE4          PIC X(4).
                      30  FILLER                      PIC X(4).
                  20  FILLER  REDEFINES PO-DRAN0175-AMATCHCODE.
                      30  PO-DRAN0175-ACODE5          PIC X(5).
                      30  FILLER                      PIC X(3).
                  20  FILLER  REDEFINES PO-DRAN0175-AMATCHCODE.
                      30  PO-DRAN0175-ACODE6          PIC X(6).
                      30  FILLER                      PIC X(2).
                  20  FILLER  REDEFINES PO-DRAN0175-AMATCHCODE.
                      30  PO-DRAN0175-ACODE7          PIC X(7).
                      30  FILLER                      PIC X(1).
      *
                  20  PO-DRAN0175-AMANRVM             PIC 9(7) COMP-3.
                  20  PO-DRAN0175-APRZVM              PIC X.
      *
                  20  PO-DRAN0175-APRODS.
                      30  PO-DRAN0175-APRODSCHL       PIC XX.
                      30  PO-DRAN0175-APRODSCHL2      PIC XX.
                      30  PO-DRAN0175-APRODSCHL3      PIC XX.
                      30  PO-DRAN0175-APRODSCHL4      PIC XX.
                      30  PO-DRAN0175-APRODSCHL5      PIC XX.
                      30  PO-DRAN0175-APRODSCHL6      PIC XX.
                      30  PO-DRAN0175-APRODSCHL7      PIC XX.
                      30  PO-DRAN0175-APRODSCHL8      PIC XX.
                      30  PO-DRAN0175-APRODSCHL9      PIC XX.
                  20  PO-DRAN0175-APROD REDEFINES PO-DRAN0175-APRODS.
                      30  PO-DRAN0175-APRS    PIC XX  OCCURS 9.
      *
                  20  PO-DRAN0175-ATARIF              PIC X(10).
                  20  PO-DRAN0175-AANREDE             PIC X.
                  20  PO-DRAN0175-AVNAME              PIC X(26).
                  20  PO-DRAN0175-AZUNAME             PIC X(26).
                  20  PO-DRAN0175-ASTR                PIC X(26).
                  20  PO-DRAN0175-AAUSL               PIC X(3).
                  20  PO-DRAN0175-APLZ                PIC X(9).
                  20  PO-DRAN0175-AORT                PIC X(21).
                  20  PO-DRAN0175-AEALTER             PIC 99.
                  20  PO-DRAN0175-ADAUER              PIC 99.
                  20  PO-DRAN0175-ABEGINN             PIC 9(7) COMP-3.
                  20  PO-DRAN0175-AABLAUF             PIC 9(7) COMP-3.
                  20  PO-DRAN0175-AZAHLW              PIC 9.
                  20  PO-DRAN0175-ABUZPROZ            PIC 99.
      *
                  20  PO-DRAN0175-AEINH.
                      30  PO-DRAN0175-AEREINH   PIC S9(6)V9   COMP-3.
                      30  PO-DRAN0175-AEREINH2  PIC S9(6)V9   COMP-3.
                      30  PO-DRAN0175-AEREINH3  PIC S9(6)V9   COMP-3.
                      30  PO-DRAN0175-AEREINH4  PIC S9(6)V9   COMP-3.
                      30  PO-DRAN0175-AEREINH5  PIC S9(6)V9   COMP-3.
                      30  PO-DRAN0175-AEREINH6  PIC S9(6)V9   COMP-3.
                      30  PO-DRAN0175-AEREINH7  PIC S9(6)V9   COMP-3.
                      30  PO-DRAN0175-AEREINH8  PIC S9(6)V9   COMP-3.
                      30  PO-DRAN0175-AEREINH9  PIC S9(6)V9   COMP-3.
                  20  PO-DRAN0175-ABEINH REDEFINES PO-DRAN0175-AEINH.
                      30  PO-DRAN0175-AABEIN    PIC S9(6)V9   COMP-3
                                                 OCCURS 9.
      *
                  20  PO-DRAN0175-AANSUMMEN.
                      30  PO-DRAN0175-AANSUMME   PIC S9(7)     COMP-3.
                      30  PO-DRAN0175-AANSUMME2  PIC S9(7)     COMP-3.
                      30  PO-DRAN0175-AANSUMME3  PIC S9(7)     COMP-3.
                      30  PO-DRAN0175-AANSUMME4  PIC S9(7)     COMP-3.
                      30  PO-DRAN0175-AANSUMME5  PIC S9(7)     COMP-3.
                      30  PO-DRAN0175-AANSUMME6  PIC S9(7)     COMP-3.
                      30  PO-DRAN0175-AANSUMME7  PIC S9(7)     COMP-3.
                      30  PO-DRAN0175-AANSUMME8  PIC S9(7)     COMP-3.
                      30  PO-DRAN0175-AANSUMME9  PIC S9(7)     COMP-3.
                  20 PO-DRAN0175-AANS  REDEFINES PO-DRAN0175-AANSUMMEN.
                      30  PO-DRAN0175-AANSUM     PIC S9(7)     COMP-3
                                                 OCCURS 9.
      *
                  20  PO-DRAN0175-AANBEITRAG     PIC S9(7)V99  COMP-3.
                  20  PO-DRAN0175-AAPEINH        PIC S9(6)V9   COMP-3.
                  20  PO-DRAN0175-AAPSUMME       PIC S9(7)     COMP-3.
                  20  PO-DRAN0175-AAPBEITRAG     PIC S9(5)V99  COMP-3.
                  20  PO-DRAN0175-ASCHNR         PIC 9(13)     COMP-3.
                  20  PO-DRAN0175-ASCHBET        PIC S9(5)V99  COMP-3.
                  20  PO-DRAN0175-ASCHECK-FELDER-NEU REDEFINES
                      PO-DRAN0175-ASCHBET.
                      30  PO-DRAN0175-ASCHECK    PIC X.
                      30  PO-DRAN0175-ASCHFILLER PIC X(3).
                  20  PO-DRAN0175-ABLTZ          PIC 9(9)     COMP-3.
                  20  PO-DRAN0175-AKTONR         PIC 9(11)    COMP-3.
      *
                  20  PO-DRAN0175-AANDATUM       PIC S9(7)    COMP-3.
                  20  PO-DRAN0175-AANERFDAT      PIC 9(7)     COMP-3.
      *
                  20  PO-DRAN0175-AANSTATUS1     PIC 99.
                  20  PO-DRAN0175-AANSTATUS2     PIC 99.
                  20  PO-DRAN0175-AANSTATUS3     PIC 99.
                  20  PO-DRAN0175-ASTATDAT1      PIC 9(7)     COMP-3.
                  20  PO-DRAN0175-ASTATDAT2      PIC 9(7)     COMP-3.
                  20  PO-DRAN0175-ASTATDAT3      PIC 9(7)     COMP-3.
                  20  PO-DRAN0175-AANART         PIC XX.
      *
                  20  PO-DRAN0175-ADIREKTION     PIC 9(3).
                  20  PO-DRAN0175-AVKBZ          PIC 9.
                  20  PO-DRAN0175-ADTADAT        PIC 9(7)     COMP-3.
                  20  PO-DRAN0175-AAG8-AEND      PIC X.
      *
                  20  PO-DRAN0175-ABETREUER      OCCURS 18.
                      30  PO-DRAN0175-AGMANR     PIC 9(7)     COMP-3.
                      30  PO-DRAN0175-AGPRZ      PIC X.
                      30  PO-DRAN0175-AGSTRSTUFE PIC 9(3)     COMP-3.
                      30  PO-DRAN0175-AGSTRTEXT  PIC X(4).
                      30  PO-DRAN0175-AGPROZ     PIC 9(4)V9   COMP-3.
      *
                  20  PO-DRAN0175-AIVSNR         PIC 9(7)     COMP-3.
                  20  PO-DRAN0175-AKRED          PIC 9(7)V99  COMP-3.
                  20  PO-DRAN0175-APROV          PIC 9(5)V99  COMP-3.
                  20  PO-DRAN0175-AABR-KZ        OCCURS 6.
                      30  PO-DRAN0175-AAB-KZ     PIC X.
      *
                  20  PO-DRAN0175-AABFAELLIG1    PIC S9(7)    COMP-3.
                  20  PO-DRAN0175-AEA40X         PIC X.
                  20  PO-DRAN0175-ABUZDAUER      PIC 99.
                  20  PO-DRAN0175-ABAVKZ         PIC X.
                  20  PO-DRAN0175-AAFOLGE        PIC X.
                  20  PO-DRAN0175-AABFAELLIG2    PIC S9(7)    COMP-3.
                  20  PO-DRAN0175-AABFAELLIG3    PIC S9(7)    COMP-3.
                  20  PO-DRAN0175-AABFAELLIG4    PIC S9(7)    COMP-3.
                  20  PO-DRAN0175-AABFAELLIG5    PIC S9(7)    COMP-3.
                  20  PO-DRAN0175-AABFAELLIG6    PIC S9(7)    COMP-3.
                  20  PO-DRAN0175-AHISKZ         PIC X.
                  20  PO-DRAN0175-AVZAN          PIC X.
                  20  PO-DRAN0175-AGEBDAT        PIC S9(7)    COMP-3.
                  20  PO-DRAN0175-AVERSDAUER-HV  PIC 9(02).
                  20  PO-DRAN0175-AEINALTER1     PIC 9(02).
                  20  PO-DRAN0175-AEINALTER2     PIC 9(02).
      *
                  20  PO-DRAN0175-FILLER         PIC X(13).

