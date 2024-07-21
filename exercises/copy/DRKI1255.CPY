      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1255.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI1255.CPv  $
      *
      *   Rev 2.2   May 03 2006 13:10:00   t13ex39
      *AKI00215: Umstellung aud EURO  LEBEN GESAMT
      *
      *   Rev 2.0   05 Jul 2001 17:06:52   p13sp99
      *- Neuerstellung -
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1255                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1255                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  10.04.2006/17:14:42
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
      *    FÜR DIE FUNKTION DRKI1255
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1255.
           05  PI-DRKI1255-KEY.


             10 PI-DRKI1255-LFOLFDNR   PIC S9(4) COMP.
             10 PI-DRKI1255-LFOVERSTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI1255-LFOVERTRAGSID   PIC X(13).


           05  PI-DRKI1255-DATEN.


             10 PI-DRKI1255-LFOBETRAG   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1255-NI-LFOBETRAG   PIC S9(4) COMP.
             10 PI-DRKI1255-LFOBETRAGWKZ   PIC X(1).
             10 PI-DRKI1255-LFODECKKAPITWKZ   PIC X(1).
             10 PI-DRKI1255-LFODECKUNGSKAPITAL   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI1255-N-LFDCKNGSKPTL   PIC S9(4) COMP.
             10 PI-DRKI1255-LFORUECKNAHMEBETR   PIC S9(3)V9(2) COMP-3.
             10 PI-DRKI1255-N-LFRCKNHMBTR   PIC S9(4) COMP.


      *
       01  PO-DRKI1255.
           05  PO-DRKI1255-RC                 PIC  X(003).
               88 PO-DRKI1255-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1255-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
