      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06.09.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3416.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI3416.CPv  $
      *
      *   Rev 2.3   May 17 2019 11:45:08   t13ac20
      *AKI01600: Erweiterung Daisy
      *
      *   Rev 2.2   Nov 28 2018 17:30:00   t13ac20
      *AKI01600: Sperren & Löschen von Verträgen - DB2 neue Tabellen
      *
      *   Rev 2.1   Sep 11 2018 12:16:20   t13ac20
      *AKI01600: Sperren & Löschen von Verträgen - DB2 neue Tabellen
      *
      *   Rev 2.0   Sep 6 2018 08:38:26 T13AC20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3416                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3416                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         14.05.2019 / 14:11:26 / T13AC20
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
      *    FÜR DIE FUNKTION DRKI3416
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3416.
           05  PI-DRKI3416-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3416-SPSLFDNR   PIC S9(4) COMP.
             10 PI-DRKI3416-SPSPARTGESELLID   PIC S9(4) COMP.
             10 PI-DRKI3416-SPSVERTRAGSNR   PIC  X(0015).
END   *
 
      *
       01  PO-DRKI3416.
           05  PO-DRKI3416-RC                     PIC  X(003).
               88 PO-DRKI3416-RC-OK               VALUE 'OK '.
               88 PO-DRKI3416-RC-NOT-FOUND        VALUE 'NFD'.
      *
ANF   *        %M_PO_DATEN_NI%
END   *
 
           05  PO-DRKI3416-DATEN.
 
 
ANF   *        %M_PO_DATEN%
             10 PO-DRKI3416-LETZTEAENDERUNG   PIC  X(0026).
             10 PO-DRKI3416-SPSAKTION-KZN   PIC  X(0001).
             10 PO-DRKI3416-SPSDAISY-KZN   PIC  X(0001).
             10 PO-DRKI3416-SPSERSTELLTTST   PIC  X(0026).
             10 PO-DRKI3416-SPSVERTRAGSID   PIC  X(0013).
END   *
 
 
      *
      ******* ENDE COPY-MEMBER ***************************************

