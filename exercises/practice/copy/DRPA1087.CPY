      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1087.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA1087.CPv  $
      *
      *   Rev 2.0   May 27 1999 12:03:24   T01JM2S
      *APA00042
      *
      *   Rev 1.0   20 Jul 1998 10:21:12   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1087                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<ZUGRIFF ART/OBJ     >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    06.10.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!06.10.98! 01 ! NEU                       ! WOLFF    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA1087
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRPA1087.
           05 PI-DRPA1087-DATEN.
             10 PI-DRPA1087-PROGRAMM                PIC  X(008).
             10 PI-DRPA1087-UFKT                    PIC S9(004) BINARY.
             10 PI-DRPA1087-KNR-PROT-ART            PIC S9(004) BINARY.
             10 PI-DRPA1087-KNR-PROT-PTR-OBJ        PIC S9(004) BINARY.
             10 PI-DRPA1087-ERSTELL-DAT-AB          PIC  X(010).
             10 PI-DRPA1087-ERSTELL-DAT-BIS         PIC  X(010).
      *
       01  PO-DRPA1087.
           05  PO-DRPA1087-RC                       PIC  X(003).
               88 PO-DRPA1087-RC-OK                       VALUE 'OK '.
               88 PO-DRPA1087-RC-EOF                      VALUE 'EOF'.
               88 PO-DRPA1087-RC-LEER                     VALUE 'LER'.
           05  PO-DRPA1087-DATEN.
               10 PO-DRPA1087-PROT.
                  COPY DRPA1091.
      *
      ******* ENDE COPY-MEMBER **************************************
