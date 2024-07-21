      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  07.09.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3436.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI3436.CPv  $
      *
      *   Rev 2.3   Nov 22 2018 10:08:40   t13ex78
      *AKI01615: OZPs für G42_DEPOTBESTAND
      *
      *   Rev 2.2   Oct 27 2018 15:20:48   t13ex78
      *AKI01615: OZPs für G42_DEPOTBESTAND
      *
      *   Rev 2.1   Oct 25 2018 14:41:40   t13ex78
      *AKI01615: OZPs für G42_DEPOTBESTAND
      *
      *   Rev 2.0   Sep 7 2018 11:16:23 T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3436                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3436                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         17.09.2018 / 09:51:36 / T13EX78
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3436
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3436.
           05  PI-DRKI3436-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3436-G42BVERTRAGSNR   PIC  X(0015).
             10 PI-DRKI3436-G42BWKN   PIC  X(0006).
END   *
 
           05  PI-DRKI3436-NI.
 
ANF   *        %M_PI_DATEN_NI%
             15 FILLER                          PIC S9(4) COMP.
END   *
 
           05  PI-DRKI3436-DATEN.
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI3436-G42BANLEGERIDOIF   PIC  X(0050).
             10 PI-DRKI3436-G42BANTEILEFREI   PIC S9(7)V9(4) COMP-3.
             10 PI-DRKI3436-G42BANTEILGESPOIF   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI3436-G42BANTEILVEFOIF   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI3436-G42BANTGEKOIF   PIC S9(7)V9(4) COMP-3.
             10 PI-DRKI3436-G42BANTGESP1   PIC S9(7)V9(4) COMP-3.
             10 PI-DRKI3436-G42BANTGESP2   PIC S9(7)V9(4) COMP-3.
             10 PI-DRKI3436-G42BANTGESP3   PIC S9(7)V9(4) COMP-3.
             10 PI-DRKI3436-G42BART   PIC  X(0001).
             10 PI-DRKI3436-G42BBESTAND   PIC S9(12)V9(4) COMP-3.
             10 PI-DRKI3436-G42BEINSTKURS   PIC S9(5)V9(2) COMP-3.
             10 PI-DRKI3436-G42BFLEINSTKURS   PIC  X(0001).
             10 PI-DRKI3436-G42BISIN   PIC  X(0012).
             10 PI-DRKI3436-G42BKAPITALAKT   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI3436-G42BKUMAUSZHALBJ   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI3436-G42BKUMAUTOWA   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI3436-G42BKUMBARAUSBR   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI3436-G42BKUMUAWIEVOR   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI3436-G42BKURSWERT   PIC S9(10)V9(2) COMP-3.
             10 PI-DRKI3436-G42BNRGWINKLBBA   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI3436-G42BNRGWINKLBBAVH   PIC S9(6)V9(2) COMP-3.
             10 PI-DRKI3436-G42BNRGWOHNEBBA   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI3436-G42BNRGWOHNEBBAVH   PIC S9(6)V9(2) COMP-3.
             10 PI-DRKI3436-G42BORGWAEH   PIC  X(0003).
             10 PI-DRKI3436-G42BREALGVERL   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI3436-G42BRUECKNAHMEPR   PIC S9(5)V9(4) COMP-3.
             10 PI-DRKI3436-G42BVLSPERRDAT1   PIC  X(0008).
             10 PI-DRKI3436-G42BVLSPERRDAT2   PIC  X(0008).
             10 PI-DRKI3436-G42BVLSPERRDAT3   PIC  X(0008).
END   *
 
      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI3436-NI1.
           05 PI-DRKI3436-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                  OCCURS 1.
       01  PI-DRKI3436-NI7.
           05 PI-DRKI3436-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                  OCCURS 1.
      *
       01  PO-DRKI3436.
           05  PO-DRKI3436-RC                     PIC  X(003).
               88 PO-DRKI3436-RC-OK               VALUE 'OK '.
               88 PO-DRKI3436-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
 
 

