      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  07.09.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3434.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI3434.CPv  $
      *
      *   Rev 2.3   Nov 22 2018 10:08:22   t13ex78
      *AKI01615: OZPs für G42_DEPOTBESTAND
      *
      *   Rev 2.2   Oct 27 2018 15:20:28   t13ex78
      *AKI01615: OZPs für G42_DEPOTBESTAND
      *
      *   Rev 2.1   Oct 25 2018 14:41:24   t13ex78
      *AKI01615: OZPs für G42_DEPOTBESTAND
      *
      *   Rev 2.0   Sep 7 2018 11:16:16 T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3434                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3434                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         17.09.2018 / 09:51:32 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI3434
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3434.
           05  PI-DRKI3434-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3434-G42BVERTRAGSNR   PIC  X(0015).
             10 PI-DRKI3434-G42BWKN   PIC  X(0006).
END   *
 
      *
       01  PO-DRKI3434.
           05  PO-DRKI3434-RC                     PIC  X(003).
               88 PO-DRKI3434-RC-OK               VALUE 'OK '.
               88 PO-DRKI3434-RC-NOT-FOUND        VALUE 'NFD'.
      *
ANF   *        %M_PO_DATEN_NI%
END   *
 
           05  PO-DRKI3434-DATEN.
 
 
ANF   *        %M_PO_DATEN%
             10 PO-DRKI3434-G42BANLEGERIDOIF   PIC  X(0050).
             10 PO-DRKI3434-G42BANTEILEFREI   PIC S9(7)V9(4) COMP-3.
             10 PO-DRKI3434-G42BANTEILGESPOIF   PIC S9(11)V9(2) COMP-3.
             10 PO-DRKI3434-G42BANTEILVEFOIF   PIC S9(11)V9(2) COMP-3.
             10 PO-DRKI3434-G42BANTGEKOIF   PIC S9(7)V9(4) COMP-3.
             10 PO-DRKI3434-G42BANTGESP1   PIC S9(7)V9(4) COMP-3.
             10 PO-DRKI3434-G42BANTGESP2   PIC S9(7)V9(4) COMP-3.
             10 PO-DRKI3434-G42BANTGESP3   PIC S9(7)V9(4) COMP-3.
             10 PO-DRKI3434-G42BART   PIC  X(0001).
             10 PO-DRKI3434-G42BBESTAND   PIC S9(12)V9(4) COMP-3.
             10 PO-DRKI3434-G42BEINSTKURS   PIC S9(5)V9(2) COMP-3.
             10 PO-DRKI3434-G42BFLEINSTKURS   PIC  X(0001).
             10 PO-DRKI3434-G42BISIN   PIC  X(0012).
             10 PO-DRKI3434-G42BKAPITALAKT   PIC S9(11)V9(2) COMP-3.
             10 PO-DRKI3434-G42BKUMAUSZHALBJ   PIC S9(11)V9(2) COMP-3.
             10 PO-DRKI3434-G42BKUMAUTOWA   PIC S9(11)V9(2) COMP-3.
             10 PO-DRKI3434-G42BKUMBARAUSBR   PIC S9(11)V9(2) COMP-3.
             10 PO-DRKI3434-G42BKUMUAWIEVOR   PIC S9(11)V9(2) COMP-3.
             10 PO-DRKI3434-G42BKURSWERT   PIC S9(10)V9(2) COMP-3.
             10 PO-DRKI3434-G42BNRGWINKLBBA   PIC S9(11)V9(2) COMP-3.
             10 PO-DRKI3434-G42BNRGWINKLBBAVH   PIC S9(6)V9(2) COMP-3.
             10 PO-DRKI3434-G42BNRGWOHNEBBA   PIC S9(11)V9(2) COMP-3.
             10 PO-DRKI3434-G42BNRGWOHNEBBAVH   PIC S9(6)V9(2) COMP-3.
             10 PO-DRKI3434-G42BORGWAEH   PIC  X(0003).
             10 PO-DRKI3434-G42BREALGVERL   PIC S9(11)V9(2) COMP-3.
             10 PO-DRKI3434-G42BRUECKNAHMEPR   PIC S9(5)V9(4) COMP-3.
             10 PO-DRKI3434-G42BVLSPERRDAT1   PIC  X(0008).
             10 PO-DRKI3434-G42BVLSPERRDAT2   PIC  X(0008).
             10 PO-DRKI3434-G42BVLSPERRDAT3   PIC  X(0008).
END   *
 
 
      *
      ******* ENDE COPY-MEMBER ***************************************
 
 

