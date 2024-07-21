      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.10.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0102.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAN0102.CPv  $
      *
      *   Rev 2.2   20 Nov 2006 16:55:04   t13ae23
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.1   Oct 27 2003 10:26:06   t13ae23
      *FAN00459 - Beheben Problematik gelöschte Datensätze -
      *
      *   Rev 2.0   Oct 22 2003 13:33:52   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0102  FUNKTION SELECT STANDARD-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: RAHNER                                          *
      *    DATUM:    22.10.2003                                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FAN00459!27.20.03  2.1! NEUERSTELLUNG             ! RAHNER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0102                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  LESEN  <<<        LESEN ANTRAGSDATEI                 *
      *                                                              *
      ****************************************************************
       01 PI-DRAN0102.
          10 PI-DRAN0102-UFKT                  PIC  X.
              88 PI-DRAN0102-UFKT-READ-FIRST   VALUE '1'.
              88 PI-DRAN0102-UFKT-READ-NEXT    VALUE '2'.
              88 PI-DRAN0102-UFKT-READ-LAST    VALUE '3'.
          10 PI-DRAN0102-VBNR                  PIC 9(007).
          10 PI-DRAN0102-GES                   PIC 9(002).
          10 PI-DRAN0102-VNR                   PIC 9(015).
      *
       01 PO-DRAN0102.
          05 PO-DRAN0102-RC                    PIC  X(003).
             88 PO-DRAN0102-RC-OK              VALUE 'OK '.
             88 PO-DRAN0102-RC-NOT-FOUND       VALUE 'NFD'.

      ***  ENTSPRICHT DEM COPY 'UU020S'
          05 PO-DRAN0102-ANSATZ-NEU.
              10  PO-DRAN0102-ASA                     PIC 99.
              10  PO-DRAN0102-ANKEY.
                  20  PO-DRAN0102-AMANR               PIC 9(7) COMP-3.
                  20  PO-DRAN0102-AGES                PIC 99.
                  20  PO-DRAN0102-AVNR                PIC 9(15) COMP-3.
      *   *
      *   *    BESTANDS-SATZ
      *   *
              10  PO-DRAN0102-ASATZ.
                  20 PO-DRAN0102-APRZMA               PIC X.
                  20  PO-DRAN0102-APRZVG              PIC XX.
      *
                  20  PO-DRAN0102-AMATCHCODE          PIC X(8).
                  20  FILLER  REDEFINES PO-DRAN0102-AMATCHCODE.
                      30  PO-DRAN0102-ACODE4          PIC X(4).
                      30  FILLER                      PIC X(4).
                  20  FILLER  REDEFINES PO-DRAN0102-AMATCHCODE.
                      30  PO-DRAN0102-ACODE5          PIC X(5).
                      30  FILLER                      PIC X(3).
                  20  FILLER  REDEFINES PO-DRAN0102-AMATCHCODE.
                      30  PO-DRAN0102-ACODE6          PIC X(6).
                      30  FILLER                      PIC X(2).
                  20  FILLER  REDEFINES PO-DRAN0102-AMATCHCODE.
                      30  PO-DRAN0102-ACODE7          PIC X(7).
                      30  FILLER                      PIC X(1).
      *
                  20  PO-DRAN0102-AMANRVM             PIC 9(7) COMP-3.
                  20  PO-DRAN0102-APRZVM              PIC X.
      *
                  20  PO-DRAN0102-APRODS.
                      30  PO-DRAN0102-APRODSCHL       PIC XX.
                      30  PO-DRAN0102-APRODSCHL2      PIC XX.
                      30  PO-DRAN0102-APRODSCHL3      PIC XX.
                      30  PO-DRAN0102-APRODSCHL4      PIC XX.
                      30  PO-DRAN0102-APRODSCHL5      PIC XX.
                      30  PO-DRAN0102-APRODSCHL6      PIC XX.
                      30  PO-DRAN0102-APRODSCHL7      PIC XX.
                      30  PO-DRAN0102-APRODSCHL8      PIC XX.
                      30  PO-DRAN0102-APRODSCHL9      PIC XX.
                  20  PO-DRAN0102-APROD REDEFINES PO-DRAN0102-APRODS.
                      30  PO-DRAN0102-APRS    PIC XX  OCCURS 9.
      *
                  20  PO-DRAN0102-ATARIF              PIC X(10).
                  20  PO-DRAN0102-AANREDE             PIC X.
                  20  PO-DRAN0102-AVNAME              PIC X(26).
                  20  PO-DRAN0102-AZUNAME             PIC X(26).
                  20  PO-DRAN0102-ASTR                PIC X(26).
                  20  PO-DRAN0102-AAUSL               PIC X(3).
                  20  PO-DRAN0102-APLZ                PIC X(9).
                  20  PO-DRAN0102-AORT                PIC X(21).
                  20  PO-DRAN0102-AEALTER             PIC 99.
                  20  PO-DRAN0102-ADAUER              PIC 99.
                  20  PO-DRAN0102-ABEGINN             PIC 9(7) COMP-3.
                  20  PO-DRAN0102-AABLAUF             PIC 9(7) COMP-3.
                  20  PO-DRAN0102-AZAHLW              PIC 9.
                  20  PO-DRAN0102-ABUZPROZ            PIC 99.
      *
                  20  PO-DRAN0102-AEINH.
                      30  PO-DRAN0102-AEREINH   PIC S9(6)V9   COMP-3.
                      30  PO-DRAN0102-AEREINH2  PIC S9(6)V9   COMP-3.
                      30  PO-DRAN0102-AEREINH3  PIC S9(6)V9   COMP-3.
                      30  PO-DRAN0102-AEREINH4  PIC S9(6)V9   COMP-3.
                      30  PO-DRAN0102-AEREINH5  PIC S9(6)V9   COMP-3.
                      30  PO-DRAN0102-AEREINH6  PIC S9(6)V9   COMP-3.
                      30  PO-DRAN0102-AEREINH7  PIC S9(6)V9   COMP-3.
                      30  PO-DRAN0102-AEREINH8  PIC S9(6)V9   COMP-3.
                      30  PO-DRAN0102-AEREINH9  PIC S9(6)V9   COMP-3.
                  20  PO-DRAN0102-ABEINH REDEFINES PO-DRAN0102-AEINH.
                      30  PO-DRAN0102-AABEIN    PIC S9(6)V9   COMP-3
                                                 OCCURS 9.
      *
                  20  PO-DRAN0102-AANSUMMEN.
                      30  PO-DRAN0102-AANSUMME   PIC S9(7)     COMP-3.
                      30  PO-DRAN0102-AANSUMME2  PIC S9(7)     COMP-3.
                      30  PO-DRAN0102-AANSUMME3  PIC S9(7)     COMP-3.
                      30  PO-DRAN0102-AANSUMME4  PIC S9(7)     COMP-3.
                      30  PO-DRAN0102-AANSUMME5  PIC S9(7)     COMP-3.
                      30  PO-DRAN0102-AANSUMME6  PIC S9(7)     COMP-3.
                      30  PO-DRAN0102-AANSUMME7  PIC S9(7)     COMP-3.
                      30  PO-DRAN0102-AANSUMME8  PIC S9(7)     COMP-3.
                      30  PO-DRAN0102-AANSUMME9  PIC S9(7)     COMP-3.
                  20 PO-DRAN0102-AANS  REDEFINES PO-DRAN0102-AANSUMMEN.
                      30  PO-DRAN0102-AANSUM     PIC S9(7)     COMP-3
                                                 OCCURS 9.
      *
                  20  PO-DRAN0102-AANBEITRAG     PIC S9(7)V99  COMP-3.
                  20  PO-DRAN0102-AAPEINH        PIC S9(6)V9   COMP-3.
                  20  PO-DRAN0102-AAPSUMME       PIC S9(7)     COMP-3.
                  20  PO-DRAN0102-AAPBEITRAG     PIC S9(5)V99  COMP-3.
                  20  PO-DRAN0102-ASCHNR         PIC 9(13)     COMP-3.
                  20  PO-DRAN0102-ASCHBET        PIC S9(5)V99  COMP-3.
                  20  PO-DRAN0102-ASCHECK-FELDER-NEU REDEFINES
                      PO-DRAN0102-ASCHBET.
                      30  PO-DRAN0102-ASCHECK    PIC X.
                      30  PO-DRAN0102-ASCHFILLER PIC X(3).
                  20  PO-DRAN0102-ABLTZ          PIC 9(9)     COMP-3.
                  20  PO-DRAN0102-AKTONR         PIC 9(11)    COMP-3.
      *
                  20  PO-DRAN0102-AANDATUM       PIC S9(7)    COMP-3.
                  20  PO-DRAN0102-AANERFDAT      PIC 9(7)     COMP-3.
      *
                  20  PO-DRAN0102-AANSTATUS1     PIC 99.
                  20  PO-DRAN0102-AANSTATUS2     PIC 99.
                  20  PO-DRAN0102-AANSTATUS3     PIC 99.
                  20  PO-DRAN0102-ASTATDAT1      PIC 9(7)     COMP-3.
                  20  PO-DRAN0102-ASTATDAT2      PIC 9(7)     COMP-3.
                  20  PO-DRAN0102-ASTATDAT3      PIC 9(7)     COMP-3.
                  20  PO-DRAN0102-AANART         PIC XX.
      *
                  20  PO-DRAN0102-ADIREKTION     PIC 9(3).
                  20  PO-DRAN0102-AVKBZ          PIC 9.
                  20  PO-DRAN0102-ADTADAT        PIC 9(7)     COMP-3.
                  20  PO-DRAN0102-AAG8-AEND      PIC X.
      *
                  20  PO-DRAN0102-ABETREUER      OCCURS 18.
                      30  PO-DRAN0102-AGMANR     PIC 9(7)     COMP-3.
                      30  PO-DRAN0102-AGPRZ      PIC X.
                      30  PO-DRAN0102-AGSTRSTUFE PIC 9(3)     COMP-3.
                      30  PO-DRAN0102-AGSTRTEXT  PIC X(4).
                      30  PO-DRAN0102-AGPROZ     PIC 9(4)V9   COMP-3.
      *
                  20  PO-DRAN0102-AIVSNR         PIC 9(7)     COMP-3.
                  20  PO-DRAN0102-AKRED          PIC 9(7)V99  COMP-3.
                  20  PO-DRAN0102-APROV          PIC 9(5)V99  COMP-3.
                  20  PO-DRAN0102-AABR-KZ        OCCURS 6.
                      30  PO-DRAN0102-AAB-KZ     PIC X.
      *
                  20  PO-DRAN0102-AABFAELLIG1    PIC S9(7)    COMP-3.
                  20  PO-DRAN0102-AEA40X         PIC X.
                  20  PO-DRAN0102-ABUZDAUER      PIC 99.
                  20  PO-DRAN0102-ABAVKZ         PIC X.
                  20  PO-DRAN0102-AAFOLGE        PIC X.
                  20  PO-DRAN0102-AABFAELLIG2    PIC S9(7)    COMP-3.
                  20  PO-DRAN0102-AABFAELLIG3    PIC S9(7)    COMP-3.
                  20  PO-DRAN0102-AABFAELLIG4    PIC S9(7)    COMP-3.
                  20  PO-DRAN0102-AABFAELLIG5    PIC S9(7)    COMP-3.
                  20  PO-DRAN0102-AABFAELLIG6    PIC S9(7)    COMP-3.
                  20  PO-DRAN0102-AHISKZ         PIC X.
                  20  PO-DRAN0102-AVZAN          PIC X.
                  20  PO-DRAN0102-AGEBDAT        PIC S9(7)    COMP-3.
                  20  PO-DRAN0102-AVERSDAUER-HV  PIC 9(02).
                  20  PO-DRAN0102-AEINALTER1     PIC 9(02).
                  20  PO-DRAN0102-AEINALTER2     PIC 9(02).
      *
                  20  PO-DRAN0102-FILLER         PIC X(13).
      *
