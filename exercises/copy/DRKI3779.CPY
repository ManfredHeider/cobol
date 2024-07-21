      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.04.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3779.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3779.CPv  $
      *
      *   Rev 2.2   Oct 06 2020 16:38:06   u130684
      *AKI01773: Anbindung Compeon an DV - OZPs
      *
      *   Rev 2.1   Jun 23 2020 11:55:30   t13ex27
      *AKI01773: Anbindung Compeon an DV - OZPs
      *
      *   Rev 2.0   Apr 14 2020 15:59:31 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3779                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3779                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         30.04.2020 / 15:38:22 / T13EX27
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
      *    FÜR DIE FUNKTION DRKI3779
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3779.
           05  PI-DRKI3779-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI3779-G90CVVERTRAGSNR   PIC  X(0015).
END   *

      *
       01  PO-DRKI3779.
           05  PO-DRKI3779-RC                     PIC  X(003).
               88 PO-DRKI3779-RC-OK               VALUE 'OK '.
               88 PO-DRKI3779-RC-NOT-FOUND        VALUE 'NFD'.
      *
ANF   *        %M_PO_DATEN_NI%
END   *

           05  PO-DRKI3779-DATEN.


ANF   *        %M_PO_DATEN%
             10 PO-DRKI3779-G90CVABSCLVBNR   PIC S9(9) COMP.
             10 PO-DRKI3779-G90CVAENDKZ   PIC  X(0002).
             10 PO-DRKI3779-G90CVAKTSALDO   PIC S9(12)V9(2) COMP-3.
             10 PO-DRKI3779-G90CVAUSZAHL   PIC  X(0008).
             10 PO-DRKI3779-G90CVBIC   PIC  X(0011).
             10 PO-DRKI3779-G90CVDELETEKZ   PIC  X(0001).
             10 PO-DRKI3779-G90CVFAELLIG   PIC  X(0008).
             10 PO-DRKI3779-G90CVFINBETR   PIC S9(12)V9(2) COMP-3.
             10 PO-DRKI3779-G90CVGESID   PIC S9(4) COMP.
             10 PO-DRKI3779-G90CVIBAN   PIC  X(0034).
             10 PO-DRKI3779-G90CVIBANKREDIT   PIC  X(0034).
             10 PO-DRKI3779-G90CVKTOINHNNAME   PIC  X(0030).
             10 PO-DRKI3779-G90CVKTOINHVNAME   PIC  X(0030).
             10 PO-DRKI3779-G90CVMANDANT   PIC S9(9) COMP.
             10 PO-DRKI3779-G90CVMANDAT   PIC  X(0035).
             10 PO-DRKI3779-G90CVPRODUKTART   PIC  X(0001).
             10 PO-DRKI3779-G90CVPRODUKTGEB   PIC  X(0030).
             10 PO-DRKI3779-G90CVRATE   PIC S9(12)V9(2) COMP-3.
             10 PO-DRKI3779-G90CVRESTLAUF   PIC S9(3)V COMP-3.
             10 PO-DRKI3779-G90CVSCHLIESS   PIC  X(0008).
             10 PO-DRKI3779-G90CVSOLLZINS   PIC S9(2)V9(4) COMP-3.
             10 PO-DRKI3779-G90CVVEROEFF   PIC  X(0008).
             10 PO-DRKI3779-G90CVZUSAGE   PIC  X(0008).
END   *


      *
      ******* ENDE COPY-MEMBER ***************************************


