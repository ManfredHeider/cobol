      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06.09.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3418.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI3418.CPv  $
      *
      *   Rev 2.3   May 17 2019 11:46:26   t13ac20
      *AKI01600: Erweiterung Daisy
      *
      *   Rev 2.2   Nov 28 2018 17:31:04   t13ac20
      *AKI01600: Sperren & Löschen von Verträgen - DB2 neue Tabellen
      *
      *   Rev 2.1   Sep 11 2018 12:17:02   t13ac20
      *AKI01600: Sperren & Löschen von Verträgen - DB2 neue Tabellen
      *
      *   Rev 2.0   Sep 6 2018 08:38:29 T13AC20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3418                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3418                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         14.05.2019 / 14:11:29 / T13AC20
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
      *    FÜR DIE FUNKTION DRKI3418
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3418.
           05  PI-DRKI3418-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3418-SPSLFDNR   PIC S9(4) COMP.
             10 PI-DRKI3418-SPSPARTGESELLID   PIC S9(4) COMP.
             10 PI-DRKI3418-SPSVERTRAGSNR   PIC  X(0015).
END   *
 
           05  PI-DRKI3418-NI.
 
ANF   *        %M_PI_DATEN_NI%
             15 PI-DRKI3418-NI-SPSAKTION-KZN    PIC S9(4) COMP.
             15 PI-DRKI3418-NI-SPSDAISY-KZN     PIC S9(4) COMP.
             15 PI-DRKI3418-NI-SPSERSTELLTTST   PIC S9(4) COMP.
             15 PI-DRKI3418-NI-SPSVERTRAGSID    PIC S9(4) COMP.
END   *
 
           05  PI-DRKI3418-DATEN.
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI3418-SPSAKTION-KZN   PIC  X(0001).
             10 PI-DRKI3418-SPSDAISY-KZN   PIC  X(0001).
             10 PI-DRKI3418-SPSERSTELLTTST   PIC  X(0026).
             10 PI-DRKI3418-SPSVERTRAGSID   PIC  X(0013).
END   *
 
      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI3418-NI1.
           05 PI-DRKI3418-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                  OCCURS 4.
       01  PI-DRKI3418-NI7.
           05 PI-DRKI3418-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                  OCCURS 4.
      *
       01  PO-DRKI3418.
           05  PO-DRKI3418-RC                     PIC  X(003).
               88 PO-DRKI3418-RC-OK               VALUE 'OK '.
               88 PO-DRKI3418-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************

