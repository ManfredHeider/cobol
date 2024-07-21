      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1283.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI1283.CPv  $
      *
      *   Rev 2.2   Mar 04 2010 17:21:58   t13ex48
      *AKI00920: Teilbeträge für Hauptversicherung: Cobol-Sourcen
      *
      *   Rev 2.0   05 Jul 2001 17:07:20   p13sp99
      *- Neuerstellung -
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1283                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1283                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  26.02.2010/10:18:57
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
010310*AKI00920 !01.03.10!0002! Neue und geaenderte Feld- ! HSE      *
      *         !        !    ! namen in LEBENTEILSCHICHT !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI1283
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1283.
           05  PI-DRKI1283-KEY.


             10 PI-DRKI1283-LTSTTNR   PIC S9(4) COMP.
             10 PI-DRKI1283-LTSVERSTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI1283-LTSVERTRAGSID   PIC X(13).
             10 PI-DRKI1283-LTSZAHLDYNERHOEH   PIC S9(4) COMP.
             
      *
       01  PO-DRKI1283.
           05  PO-DRKI1283-RC                 PIC  X(003).
               88 PO-DRKI1283-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1283-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI1283-DATEN.


             10 PO-DRKI1283-LTSBEITRZUSTID   PIC X(4).
             10 PO-DRKI1283-LTSDATDYNERHOEH   PIC X(10).
             10 PO-DRKI1283-LTSSCHICHTBEITR   PIC S9(11)V9(2) COMP-3.
             10 PO-DRKI1283-LTSTARBEZEICHNUNG   PIC X(15).
             10 PO-DRKI1283-LTSTTYPBEITR   PIC S9(11)V9(2) COMP-3.
             10 PO-DRKI1283-LTSVERSSUMME   PIC S9(11)V9(2) COMP-3.
             10 PO-DRKI1283-NI-LTSVERSSUMME   PIC S9(4) COMP.
             10 PO-DRKI1283-LTSVERTRZUSTANDDAT   PIC X(10).
             10 PO-DRKI1283-LTSVERTRZUSTANDID   PIC S9(4) COMP.
             10 PO-DRKI1283-N-LTSVRTRZSTNDD   PIC S9(4) COMP.
             10 PO-DRKI1283-LTSWKZ   PIC X(1).
             

      *
      ******* ENDE COPY-MEMBER **************************************
