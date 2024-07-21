      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.04.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3777.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3777.CPv  $
      *
      *   Rev 2.2   Oct 06 2020 16:37:54   u130684
      *AKI01773: Anbindung Compeon an DV - OZPs
      *
      *   Rev 2.1   Jun 23 2020 11:53:52   t13ex27
      *AKI01773: Anbindung Compeon an DV - OZPs
      *
      *   Rev 2.0   Apr 14 2020 15:59:26 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3777                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3777                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         30.04.2020 / 15:38:17 / T13EX27
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
      *    FÜR DIE FUNKTION DRKI3777
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3777.
           05  PI-DRKI3777-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI3777-G90CVVERTRAGSNR   PIC  X(0015).
END   *

           05  PI-DRKI3777-NI.

ANF   *        %M_PI_DATEN_NI%
             15 PI-DRKI3777-NI-G90CVABSCLVBNR   PIC S9(4) COMP.
             15 PI-DRKI3777-NI-G90CVAENDKZ      PIC S9(4) COMP.
             15 PI-DRKI3777-NI-G90CVAKTSALDO    PIC S9(4) COMP.
             15 PI-DRKI3777-NI-G90CVAUSZAHL     PIC S9(4) COMP.
             15 PI-DRKI3777-NI-G90CVBIC         PIC S9(4) COMP.
             15 PI-DRKI3777-NI-G90CVDELETEKZ    PIC S9(4) COMP.
             15 PI-DRKI3777-NI-G90CVFAELLIG     PIC S9(4) COMP.
             15 PI-DRKI3777-NI-G90CVFINBETR     PIC S9(4) COMP.
             15 PI-DRKI3777-NI-G90CVGESID       PIC S9(4) COMP.
             15 PI-DRKI3777-NI-G90CVIBAN        PIC S9(4) COMP.
             15 PI-DRKI3777-NI-G90CVIBANKREDIT  PIC S9(4) COMP.
             15 PI-DRKI3777-N-G90CVKTNHNNM      PIC S9(4) COMP.
             15 PI-DRKI3777-N-G90CVKTNHVNM      PIC S9(4) COMP.
             15 PI-DRKI3777-NI-G90CVMANDANT     PIC S9(4) COMP.
             15 PI-DRKI3777-NI-G90CVMANDAT      PIC S9(4) COMP.
             15 PI-DRKI3777-NI-G90CVPRODUKTART  PIC S9(4) COMP.
             15 PI-DRKI3777-NI-G90CVPRODUKTGEB  PIC S9(4) COMP.
             15 PI-DRKI3777-NI-G90CVRATE        PIC S9(4) COMP.
             15 PI-DRKI3777-NI-G90CVRESTLAUF    PIC S9(4) COMP.
             15 PI-DRKI3777-NI-G90CVSCHLIESS    PIC S9(4) COMP.
             15 PI-DRKI3777-NI-G90CVSOLLZINS    PIC S9(4) COMP.
             15 PI-DRKI3777-NI-G90CVVEROEFF     PIC S9(4) COMP.
             15 PI-DRKI3777-NI-G90CVZUSAGE      PIC S9(4) COMP.
END   *

           05  PI-DRKI3777-DATEN.

ANF   *        %M_PI_DATEN%
             10 PI-DRKI3777-G90CVABSCLVBNR   PIC S9(9) COMP.
             10 PI-DRKI3777-G90CVAENDKZ   PIC  X(0002).
             10 PI-DRKI3777-G90CVAKTSALDO   PIC S9(12)V9(2) COMP-3.
             10 PI-DRKI3777-G90CVAUSZAHL   PIC  X(0008).
             10 PI-DRKI3777-G90CVBIC   PIC  X(0011).
             10 PI-DRKI3777-G90CVDELETEKZ   PIC  X(0001).
             10 PI-DRKI3777-G90CVFAELLIG   PIC  X(0008).
             10 PI-DRKI3777-G90CVFINBETR   PIC S9(12)V9(2) COMP-3.
             10 PI-DRKI3777-G90CVGESID   PIC S9(4) COMP.
             10 PI-DRKI3777-G90CVIBAN   PIC  X(0034).
             10 PI-DRKI3777-G90CVIBANKREDIT   PIC  X(0034).
             10 PI-DRKI3777-G90CVKTOINHNNAME   PIC  X(0030).
             10 PI-DRKI3777-G90CVKTOINHVNAME   PIC  X(0030).
             10 PI-DRKI3777-G90CVMANDANT   PIC S9(9) COMP.
             10 PI-DRKI3777-G90CVMANDAT   PIC  X(0035).
             10 PI-DRKI3777-G90CVPRODUKTART   PIC  X(0001).
             10 PI-DRKI3777-G90CVPRODUKTGEB   PIC  X(0030).
             10 PI-DRKI3777-G90CVRATE   PIC S9(12)V9(2) COMP-3.
             10 PI-DRKI3777-G90CVRESTLAUF   PIC S9(3)V COMP-3.
             10 PI-DRKI3777-G90CVSCHLIESS   PIC  X(0008).
             10 PI-DRKI3777-G90CVSOLLZINS   PIC S9(2)V9(4) COMP-3.
             10 PI-DRKI3777-G90CVVEROEFF   PIC  X(0008).
             10 PI-DRKI3777-G90CVZUSAGE   PIC  X(0008).
END   *

      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI3777-NI1.
           05 PI-DRKI3777-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                  OCCURS 23.
       01  PI-DRKI3777-NI7.
           05 PI-DRKI3777-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                  OCCURS 23.
      *
       01  PO-DRKI3777.
           05  PO-DRKI3777-RC                     PIC  X(003).
               88 PO-DRKI3777-RC-OK               VALUE 'OK '.
               88 PO-DRKI3777-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************


