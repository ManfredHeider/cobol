      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06.09.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3411.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI3411.CPv  $
      *
      *   Rev 2.3   Nov 21 2019 08:21:56   t13ex27
      *AKI01674: DSGVO: Mappen der DB2-Tabelle zu gesperrten/gelösc
      *hten Verträge in die OKI
      *
      *   Rev 2.2   Nov 28 2018 17:27:18   t13ac20
      *AKI01600: Sperren & Löschen von Verträgen - DB2 neue Tabellen
      *
      *   Rev 2.1   Sep 11 2018 12:14:24   t13ac20
      *AKI01600: Sperren & Löschen von Verträgen - DB2 neue Tabellen
      *
      *   Rev 2.0   Sep 6 2018 08:09:10 T13AC20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3411                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3411                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         17.09.2019 / 16:41:03 / T13EX27
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
      *    FÜR DIE FUNKTION DRKI3411
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3411.
           05  PI-DRKI3411-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI3411-SPALFDNR   PIC S9(4) COMP.
             10 PI-DRKI3411-SPAPARTGESELLID   PIC S9(4) COMP.
             10 PI-DRKI3411-SPAVERTRAGSNR   PIC  X(0015).
END   *

      *
       01  PO-DRKI3411.
           05  PO-DRKI3411-RC                     PIC  X(003).
               88 PO-DRKI3411-RC-OK               VALUE 'OK '.
               88 PO-DRKI3411-RC-NOT-FOUND        VALUE 'NFD'.
      *
ANF   *        %M_PO_DATEN_NI%
END   *

           05  PO-DRKI3411-DATEN.


ANF   *        %M_PO_DATEN%
             10 PO-DRKI3411-SPAARCHIVTST   PIC  X(0026).
             10 PO-DRKI3411-SPAENTSPERRTST   PIC  X(0026).
             10 PO-DRKI3411-SPAGRUND   PIC  X(0001).
             10 PO-DRKI3411-SPALOESCHTST   PIC  X(0026).
             10 PO-DRKI3411-SPASPERRDAT   PIC  X(010).
             10 PO-DRKI3411-SPASTATUS-KNR   PIC S9(4) COMP.
             10 PO-DRKI3411-SPASTATUS-KTY   PIC S9(4) COMP.
             10 PO-DRKI3411-SPAVERTRAGSID   PIC  X(0013).
END   *


      *
      ******* ENDE COPY-MEMBER ***************************************

