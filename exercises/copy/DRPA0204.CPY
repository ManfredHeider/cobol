      * T13AE19   Stand: 05/02/97    16:34:04
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0204.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0204.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:28:16   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   06 Aug 1998 15:04:16   T01JM2S
      *APAA0032 - PEW-Release 2.0
      *
      *   Rev 1.1   Apr 09 1997 16:51:38   T13AE31
      *EINFUEHRUNG PEW
      *
      *   Rev 1.0   Feb 19 1997 12:09:18   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0204                                      *
      *    --------------------------------------------------------   *
      *    DATEN-SCHNITTSTELLE FÜR DIE FUNKTION DRPA0204              *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BORK                                             *
      *    DATUM:    24.01.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !24.01.97! 01 ! NEU                       ! BORK     *
      *         !        !    !                           !          *
      * PEW-REL.!20.05.98! 02 ! ÜBERARBEITUNG PEW-RELEASE ! WOLFF    *
      * 2.0     !        !    ! 2.0                       !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA0204
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT-ALL  <<<
      *
      ****************************************************************
      *
       01  PI-DRPA0204.
           05  PI-DRPA0204-UFKT                     PIC  X(002).
           05  PI-DRPA0204-AUSWAHL-KRITERIEN.
               10 PI-DRPA0204-PEWA-ID               PIC  9(015).
      *
       01  PO-DRPA0204.
           05  PO-DRPA0204-RC                       PIC  X(003).
               88 PO-DRPA0204-RC-OK                       VALUE 'OK '.
               88 PO-DRPA0204-RC-EOF                      VALUE 'EOF'.
               88 PO-DRPA0204-RC-LEER                     VALUE 'LER'.

200598     05  PO-DRPA0204-DATEN.
               10 PO-DRPA0204-ERGEBNISZEILE.
200598            15 PO-DRPA0204-PEW-PRODUKT.
200598               COPY DRPA0960.
                  15 PO-DRPA0204-PEWP-LAST-UPDATE   PIC  X(026).
                  15 PO-DRPA0204-PEWP-KENNUNG       PIC  X(008).
      *
      ******* ENDE COPY-MEMBER **************************************
