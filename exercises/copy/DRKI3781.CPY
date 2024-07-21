      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  14.04.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3781.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI3781.CPv  $
      *
      *   Rev 2.3   Oct 06 2020 16:38:18   u130684
      *AKI01773: Anbindung Compeon an DV - OZPs
      *
      *   Rev 2.2   Oct 06 2020 11:50:26   u130684
      *AKI01773: Anbindung Compeon an DV - OZPs
      *
      *   Rev 2.1   Jun 23 2020 11:57:00   t13ex27
      *AKI01773: Anbindung Compeon an DV - OZPs
      *
      *   Rev 2.0   Apr 14 2020 14:50:24   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3781                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3781                    *
      *    --------------------------------------------------------   *
      ****************************************************************
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
      *    FÜR DIE FUNKTION DRKI3781
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3781.
           05 PI-DRKI3781-PROGRAMM              PIC  X(008).
           05 PI-DRKI3781-UFKT-CODE             PIC  9(002).
                88 PI-DRKI3781-UFKT-FIRST               VALUE 1.
                88 PI-DRKI3781-UFKT-NEXT                VALUE 2.
                88 PI-DRKI3781-UFKT-QUIT                VALUE 3.
      *
           05  PI-DRKI3781-KEY.
                10 PI-DRKI3781-G90CVMANDANT     PIC S9(9) COMP.
      *
       01  PO-DRKI3781.
           05  PO-DRKI3781-RC                   PIC  X(003).
                88 PO-DRKI3781-RC-OK                    VALUE 'OK '.
                88 PO-DRKI3781-RC-LEER                  VALUE 'LER'.
                88 PO-DRKI3781-RC-EOF                   VALUE 'EOF'.
      *
           05  DRKI3781-DATEN.
             10 PO-DRKI3781-G90CVVERTRAGSNR      PIC X(15).
             10 PO-DRKI3781-G90CVAENDKZ          PIC X(2).
             10 PO-DRKI3781-G90CVGESID           PIC S9(4) USAGE COMP.
             10 PO-DRKI3781-G90CVABSCLVBNR       PIC S9(9) USAGE COMP.
             10 PO-DRKI3781-G90CVVEROEFF         PIC X(8).
             10 PO-DRKI3781-G90CVSCHLIESS        PIC X(8).
             10 PO-DRKI3781-G90CVFINBETR  PIC S9(12)V9(2) USAGE COMP-3.
             10 PO-DRKI3781-G90CVZUSAGE          PIC X(8).
             10 PO-DRKI3781-G90CVAUSZAHL         PIC X(8).
             10 PO-DRKI3781-G90CVPRODUKTGEB      PIC X(30).
             10 PO-DRKI3781-G90CVPRODUKTART      PIC X(1).
             10 PO-DRKI3781-G90CVAKTSALDO PIC S9(12)V9(2) USAGE COMP-3.
             10 PO-DRKI3781-G90CVSOLLZINS  PIC S9(2)V9(4) USAGE COMP-3.
             10 PO-DRKI3781-G90CVRATE     PIC S9(12)V9(2) USAGE COMP-3.
             10 PO-DRKI3781-G90CVFAELLIG         PIC X(8).
             10 PO-DRKI3781-G90CVRESTLAUF      PIC S9(3)V USAGE COMP-3.
             10 PO-DRKI3781-G90CVDELETEKZ        PIC X(1).
             10 PO-DRKI3781-G90CVIBAN            PIC X(34).
             10 PO-DRKI3781-G90CVBIC             PIC X(11).
             10 PO-DRKI3781-G90CVMANDAT          PIC X(35).
             10 PO-DRKI3781-G90CVKTOINHNNAME     PIC X(30).
             10 PO-DRKI3781-G90CVKTOINHVNAME     PIC X(30).
             10 PO-DRKI3781-G90CVIBANKREDIT      PIC X(34).
      *
      ******* ENDE COPY-MEMBER ***************************************


