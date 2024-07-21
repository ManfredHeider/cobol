      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1412.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA1412.CPv  $
      *
      *   Rev 2.3   Mar 07 2011 10:59:16   t13ae37
      *APA01171: I191-ausgelagerte Bewegungungen heranziehen
      *
      *   Rev 2.2   Jan 30 2002 17:42:00   T13ex22
      *APA00173: Abweichung bei Vorbewegungsermittlung zugelassen
      *
      *   Rev 2.1   30 Aug 2000 11:15:04   T13EX22
      *APA00105: PVW III
      *
      *   Rev 2.0   12 Jul 2000 12:32:40   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1412                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA1412                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: MUND                                             *
      *    DATUM:    07.08.2000                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00105!07.08.00!2.0 ! NEU                       ! MUND     *
      * APA00173!11.01.02!2.1 ! ABWEICHUNG ERGÄNZT        ! MUND     *
      * APA01171!14.02.11!2.2 ! KZ FÜR PROV. HISTORIE     ! BUCH     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA1412
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRPA1412.
           05 PI-DRPA1412-UFKT             PIC  X(003).
              88 PI-DRPA1412-EINMALZAHLUNG    VALUE 'EZ '.
              88 PI-DRPA1412-SPARPLAN         VALUE 'SP '.

           05 PI-DRPA1412-PRINTE          PIC S9(015) PACKED-DECIMAL.
           05 PI-DRPA1412-PGEB-GES        PIC S9(004) BINARY.
           05 PI-DRPA1412-PROD-PROD-SCHL  PIC S9(004) BINARY.
           05 PI-DRPA1412-FAELLIG-VON     PIC  X(010).
           05 PI-DRPA1412-GSMT-PROV   PIC S9(009)V9(2) PACKED-DECIMAL.
110102     05 PI-DRPA1412-ABWEICHUNG-EINZEL
110102                                   PIC S9(002)V99 PACKED-DECIMAL.
110102     05 PI-DRPA1412-ABWEICHUNG-SUM PIC S9(002)V99 PACKED-DECIMAL.

140211     05 PI-DRPA1412-KZ-PROVISION   PIC  X(001).
140211        88 PI-DRPA1412-KZ-BESTAND                   VALUE SPACE.
140211        88 PI-DRPA1412-KZ-HISTORIE                  VALUE '1'.


       01  PO-DRPA1412.
           05 PO-DRPA1412-RC              PIC  X(003).
              88 PO-DRPA1412-RC-OK                        VALUE 'OK '.
              88 PO-DRPA1412-RC-NOT-FOUND                 VALUE 'NFD'.
      *

