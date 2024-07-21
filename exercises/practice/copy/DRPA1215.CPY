      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa1215.cpy  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:\MF\CPY\drpa1215.cpv  $
      *
      *   Rev 2.3   04 Oct 1999 15:11:56   T01JM2S
      *APA00057 - EINBAU MODUS-PROTOKOLL IN PK10
      *
      *   Rev 2.2   Apr 19 1999 10:12:42   T13EX21
      *APA00012 - frei (kann benutzt werden)
      *
      *   Rev 2.1   Apr 15 1999 17:44:26   T13EX21
      * 
      *
      *   Rev 2.0   Mar 30 1999 08:44:04   P13SP99
      *Neuerstellung
      *
      *   Rev 2.0   Mar 05 1999 09:20:24   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1215                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<ZUGRIFF PTR-ID      >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    01.10.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00057!01.10.99!2.3 ! NEU                       ! WOLFF    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA1215
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRPA1215.
           05 PI-DRPA1215-DATEN.
             10 PI-DRPA1215-PROGRAMM                PIC  X(008).
             10 PI-DRPA1215-UFKT                    PIC S9(004) BINARY.
             10 PI-DRPA1215-PROT-PTR-ID             PIC S9(015)
                                                        PACKED-DECIMAL.
      *
       01  PO-DRPA1215.
           05  PO-DRPA1215-RC                       PIC  X(003).
               88 PO-DRPA1215-RC-OK                       VALUE 'OK '.
               88 PO-DRPA1215-RC-EOF                      VALUE 'EOF'.
               88 PO-DRPA1215-RC-LEER                     VALUE 'LER'.
           05  PO-DRPA1215-DATEN.
               10 PO-DRPA1215-PROT.
                  COPY DRPA1091.
      *
      ******* ENDE COPY-MEMBER **************************************
