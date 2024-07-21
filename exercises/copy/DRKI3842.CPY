      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.09.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3842.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3842.CPv  $
      *
      *   Rev 2.1   Nov 23 2020 17:16:48   t13ex39
      *AKI01790: Deutsche Bank - Depots KSDS--->TT
      *B E S T A N D
      *
      *   Rev 2.0   Sep 2 2020 14:48:03 T13EX39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3842                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3842                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         02.09.2020 / 14:48:03 / T13EX39
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
      *    FÜR DIE FUNKTION DRKI3842
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3842.
           05  PI-DRKI3842-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI3842-G48DBUNTERNR   PIC  X(0002).
             10 PI-DRKI3842-G48DBVNR   PIC  X(0015).
             10 PI-DRKI3842-G48DBWAEHRUNGSKZ   PIC  X(0003).
             10 PI-DRKI3842-G48DBWPKENNNR   PIC  X(0006).
END   *

           05  PI-DRKI3842-NI.

ANF   *        %M_PI_DATEN_NI%
             15 FILLER                          PIC S9(4) COMP.
END   *

           05  PI-DRKI3842-DATEN.

ANF   *        %M_PI_DATEN%
             10 PI-DRKI3842-G48DBANGESPERRT1   PIC S9(7)V9(4) COMP-3.
             10 PI-DRKI3842-G48DBANGESPERRT2   PIC S9(7)V9(4) COMP-3.
             10 PI-DRKI3842-G48DBANGESPERRT3   PIC S9(7)V9(4) COMP-3.
             10 PI-DRKI3842-G48DBANLIDOIF   PIC  X(0014).
             10 PI-DRKI3842-G48DBANTEILEFREI   PIC S9(7)V9(4) COMP-3.
             10 PI-DRKI3842-G48DBANTGESPOIF   PIC S9(7)V9(4) COMP-3.
             10 PI-DRKI3842-G48DBANTLGEKOIF   PIC S9(7)V9(4) COMP-3.
             10 PI-DRKI3842-G48DBANTLVEFOIF   PIC S9(7)V9(4) COMP-3.
             10 PI-DRKI3842-G48DBANTVORJ13   PIC S9(7)V9(4) COMP-3.
             10 PI-DRKI3842-G48DBART   PIC  X(0001).
             10 PI-DRKI3842-G48DBDELKZ   PIC  X(0001).
             10 PI-DRKI3842-G48DBEINSTKURS   PIC S9(5)V9(2) COMP-3.
             10 PI-DRKI3842-G48DBEREIKONHALB   PIC S9(6)V9(2) COMP-3.
             10 PI-DRKI3842-G48DBISIN   PIC  X(0012).
             10 PI-DRKI3842-G48DBKAPITALAKT   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI3842-G48DBKAPITALVORJ   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI3842-G48DBKUBAAUBRVJ   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI3842-G48DBKUMAUS   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI3842-G48DBKUMAUSHALBJ   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI3842-G48DBKUMAUTOWA   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI3842-G48DBKUMBABR   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI3842-G48DBKUMEIN   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI3842-G48DBKUMUEGES   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI3842-G48DBKUMUEVOR   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI3842-G48DBKURSWERTEUR   PIC S9(10)V9(2) COMP-3.
             10 PI-DRKI3842-G48DBKURSWERTORG   PIC S9(12)V9(2) COMP-3.
             10 PI-DRKI3842-G48DBNRGWINKBA   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI3842-G48DBNRGWINKBAVH   PIC S9(6)V9(2) COMP-3.
             10 PI-DRKI3842-G48DBNRGWOBA   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI3842-G48DBNRGWOBAVH   PIC S9(6)V9(2) COMP-3.
             10 PI-DRKI3842-G48DBORGWKZ   PIC  X(0003).
             10 PI-DRKI3842-G48DBQUALIFLAG   PIC  X(0001).
             10 PI-DRKI3842-G48DBREALGEWVERL   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI3842-G48DBRUEBRUGES   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI3842-G48DBRUEBRUVORJ   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI3842-G48DBRUECKPR   PIC S9(5)V9(4) COMP-3.
             10 PI-DRKI3842-G48DBSTUEOWERT   PIC S9(12)V9(4) COMP-3.
             10 PI-DRKI3842-G48DBVLSPERRDAT1   PIC  X(0008).
             10 PI-DRKI3842-G48DBVLSPERRDAT2   PIC  X(0008).
             10 PI-DRKI3842-G48DBVLSPERRDAT3   PIC  X(0008).
             10 PI-DRKI3842-G48DBWPBEZ   PIC  X(0050).
END   *

      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI3842-NI1.
           05 PI-DRKI3842-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                  OCCURS 1.
       01  PI-DRKI3842-NI7.
           05 PI-DRKI3842-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                  OCCURS 1.
      *
       01  PO-DRKI3842.
           05  PO-DRKI3842-RC                     PIC  X(003).
               88 PO-DRKI3842-RC-OK               VALUE 'OK '.
               88 PO-DRKI3842-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************

