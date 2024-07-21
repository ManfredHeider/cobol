      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa1325.cpy  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\drpa1325.cpv  $
      *
      *   Rev 2.2   Mar 16 2000 17:29:42   T13EX13
      *APA00087 Umstellung auf PPGZ 
      *
      *
      *   Rev 2.1   Jan 18 2000 14:44:44   T13EX22
      *APA00071: SF44 mit TBPA0026-Zugriff
      *
      *   Rev 2.0   Dec 14 1999 16:42:14   P13SP20
      *Archiverstellung
      *
      *****************************************************************
      *    COPY-MEMBER: DRPA1325                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA1325                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: MUND                                             *
      *    DATUM:    03.01.2000                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00071!03.01.00! 01 ! NEU                       ! MUND     *
      * APA00087!16.03.00! 02 ! KORREKTUR PGVZ AUF MAX    ! WILKENS  *
      *         !        !    ! MÖGLICHE TS-QUEUE-LÄNGE   !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA1325
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION DRPA1325 DES BUFFER-OZP ZYPA0033 ERMITTELT
      *    DIE ERGEBNISMENGE FÜR TABELLE PGVZ
      *
      ****************************************************************
      *
       01  PI-DRPA1325.
           05  PI-DRPA1325-UFKT                PIC  9(02).
           05  PI-DRPA1325-LOG-SATZ-NR         PIC  9(05).
      *
       01  PO-DRPA1325.
           05  PO-DRPA1325-RC                  PIC  X(03).
               88 PO-DRPA1325-RC-OK                        VALUE 'OK '.
               88 PO-DRPA1325-RC-EOF                       VALUE 'EOF'.
               88 PO-DRPA1325-RC-LEER                      VALUE 'LER'.
           05  PO-DRPA1325-ANZAHL-ERG-ZEILEN   PIC  9(05).
          05 PO-DRPA1325-ERGEBNISTABELLE.
160300       10 PO-DRPA1325-PGVZ-TAB                     OCCURS 200.
                15  PO-DRPA1325-ERGEBNISZEILE.
                    COPY DRPA0955.
      *
      ******* ENDE COPY-MEMBER **************************************
      *---------------------------------------------------------------*
