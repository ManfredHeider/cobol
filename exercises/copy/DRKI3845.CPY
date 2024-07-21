      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  27.08.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3845.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3845.CPv  $
      *
      *   Rev 2.1   Nov 23 2020 17:17:14   t13ex39
      *AKI01790: Deutsche Bank - Depots KSDS--->TT
      *B E S T A N D
      *
      *   Rev 2.0   Aug 27 2020 17:06:34   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3845                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3813                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         25.06.2020 / 11:01:20 / T13EX39
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01789!26.06.20!    ! NEU                       ! WHO      *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3845
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  LIST  <<<
      *
      ****************************************************************
         01  PI-DRKI3845.
            05 PI-DRKI3845-UFKT-CODE             PIC  9(0002).
               88 PI-DRKI3845-UFKT-FIRST              VALUE 1.
               88 PI-DRKI3845-UFKT-NEXT               VALUE 2.
               88 PI-DRKI3845-UFKT-QUIT               VALUE 3.
 
           05 PI-DRKI3845-KEY.
              10 PI-DRKI3845-G48MANDANT           PIC  S9(9) COMP.
 
       01  PO-DRKI3845.
           05  PO-DRKI3845-KEY.


ANF   *        %M_PI_KEY%
             10 PO-DRKI3845-G48DBUNTERNR   PIC  X(0002).
             10 PO-DRKI3845-G48DBVNR   PIC  X(0015).
             10 PO-DRKI3845-G48DBWAEHRUNGSKZ   PIC  X(0003).
             10 PO-DRKI3845-G48DBWPKENNNR   PIC  X(0006).
END   *

      *
           05  PO-DRKI3845-RC                     PIC  X(003).
               88 PO-DRKI3845-RC-OK               VALUE 'OK '.
               88 PO-DRKI3845-RC-EOF              VALUE 'EOF'.
               88 PO-DRKI3845-RC-LEER             VALUE 'LER'.
      *
ANF   *        %M_PO_DATEN_NI%
END   *

           05  PO-DRKI3845-DATEN.


ANF   *        %M_PO_DATEN%
             10 PO-DRKI3845-G48DBANGESPERRT1   PIC S9(7)V9(4) COMP-3.
             10 PO-DRKI3845-G48DBANGESPERRT2   PIC S9(7)V9(4) COMP-3.
             10 PO-DRKI3845-G48DBANGESPERRT3   PIC S9(7)V9(4) COMP-3.
             10 PO-DRKI3845-G48DBANLIDOIF   PIC  X(0014).
             10 PO-DRKI3845-G48DBANTEILEFREI   PIC S9(7)V9(4) COMP-3.
             10 PO-DRKI3845-G48DBANTGESPOIF   PIC S9(7)V9(4) COMP-3.
             10 PO-DRKI3845-G48DBANTLGEKOIF   PIC S9(7)V9(4) COMP-3.
             10 PO-DRKI3845-G48DBANTLVEFOIF   PIC S9(7)V9(4) COMP-3.
             10 PO-DRKI3845-G48DBANTVORJ13   PIC S9(7)V9(4) COMP-3.
             10 PO-DRKI3845-G48DBART   PIC  X(0001).
             10 PO-DRKI3845-G48DBDELKZ   PIC  X(0001).
             10 PO-DRKI3845-G48DBEINSTKURS   PIC S9(5)V9(2) COMP-3.
             10 PO-DRKI3845-G48DBEREIKONHALB   PIC S9(6)V9(2) COMP-3.
             10 PO-DRKI3845-G48DBISIN   PIC  X(0012).
             10 PO-DRKI3845-G48DBKAPITALAKT   PIC S9(11)V9(2) COMP-3.
             10 PO-DRKI3845-G48DBKAPITALVORJ   PIC S9(11)V9(2) COMP-3.
             10 PO-DRKI3845-G48DBKUBAAUBRVJ   PIC S9(11)V9(2) COMP-3.
             10 PO-DRKI3845-G48DBKUMAUS   PIC S9(11)V9(2) COMP-3.
             10 PO-DRKI3845-G48DBKUMAUSHALBJ   PIC S9(11)V9(2) COMP-3.
             10 PO-DRKI3845-G48DBKUMAUTOWA   PIC S9(11)V9(2) COMP-3.
             10 PO-DRKI3845-G48DBKUMBABR   PIC S9(11)V9(2) COMP-3.
             10 PO-DRKI3845-G48DBKUMEIN   PIC S9(11)V9(2) COMP-3.
             10 PO-DRKI3845-G48DBKUMUEGES   PIC S9(11)V9(2) COMP-3.
             10 PO-DRKI3845-G48DBKUMUEVOR   PIC S9(11)V9(2) COMP-3.
             10 PO-DRKI3845-G48DBKURSWERTEUR   PIC S9(10)V9(2) COMP-3.
             10 PO-DRKI3845-G48DBKURSWERTORG   PIC S9(12)V9(2) COMP-3.
             10 PO-DRKI3845-G48DBNRGWINKBA   PIC S9(11)V9(2) COMP-3.
             10 PO-DRKI3845-G48DBNRGWINKBAVH   PIC S9(6)V9(2) COMP-3.
             10 PO-DRKI3845-G48DBNRGWOBA   PIC S9(11)V9(2) COMP-3.
             10 PO-DRKI3845-G48DBNRGWOBAVH   PIC S9(6)V9(2) COMP-3.
             10 PO-DRKI3845-G48DBORGWKZ   PIC  X(0003).
             10 PO-DRKI3845-G48DBQUALIFLAG   PIC  X(0001).
             10 PO-DRKI3845-G48DBREALGEWVERL   PIC S9(11)V9(2) COMP-3.
             10 PO-DRKI3845-G48DBRUEBRUGES   PIC S9(11)V9(2) COMP-3.
             10 PO-DRKI3845-G48DBRUEBRUVORJ   PIC S9(11)V9(2) COMP-3.
             10 PO-DRKI3845-G48DBRUECKPR   PIC S9(5)V9(4) COMP-3.
             10 PO-DRKI3845-G48DBSTUEOWERT   PIC S9(12)V9(4) COMP-3.
             10 PO-DRKI3845-G48DBVLSPERRDAT1   PIC  X(0008).
             10 PO-DRKI3845-G48DBVLSPERRDAT2   PIC  X(0008).
             10 PO-DRKI3845-G48DBVLSPERRDAT3   PIC  X(0008).
             10 PO-DRKI3845-G48DBWPBEZ      PIC X(50).
END   *


      *
      ******* ENDE COPY-MEMBER ***************************************

