      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1342.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRPA1342.CPv  $
      *
      *   Rev 2.2   Mar 14 2000 12:47:38   T13EX13
      *APA00087 Umstellung auf PPGZ 
      *
      *   Rev 2.1   Mar 09 2000 17:32:58   T13EX13
      *APA00087 Umstellung auf PPGZ 
      *
      *   Rev 2.0   Mar 01 2000 09:57:52   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    FUNKTION DRPA1342 DES BUFFER-OZP ZYPA0033 ERMITTELT
      *    DIE ERGEBNISMENGE FÜR TABELLE PPGZ
      *
      ****************************************************************
      *
       01  PI-DRPA1342.
           05  PI-DRPA1342-UFKT                PIC  9(02).
           05  PI-DRPA1342-LOG-SATZ-NR         PIC  9(05).
      *
       01  PO-DRPA1342.
           05  PO-DRPA1342-RC                  PIC  X(03).
               88 PO-DRPA1342-RC-OK                        VALUE 'OK '.
               88 PO-DRPA1342-RC-EOF                       VALUE 'EOF'.
               88 PO-DRPA1342-RC-LEER                      VALUE 'LER'.
           05  PO-DRPA1342-ANZAHL-ERG-ZEILEN   PIC  9(05).
          05 PO-DRPA1342-ERGEBNISTABELLE.
             10 PO-DRPA1342-PPGZ-TAB                     OCCURS 200.
                15  PO-DRPA1342-ERGEBNISZEILE.
                    COPY DRPA1344.
      *
      ******* ENDE COPY-MEMBER **************************************
      *---------------------------------------------------------------*
