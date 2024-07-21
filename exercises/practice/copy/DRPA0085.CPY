      *--------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *--------------------------------------------------------------*
      *
      *--------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *--------------------------------------------------------------*
      *    $Workfile:   DRPA0085.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0085.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:27:40   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 06 1997 14:56:28   P13PVCS
      *Neuerstellung
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0085  Funktion Select Standard-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Priggen                                         *
      *    DATUM:    20.08.1996                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !20.08.96! 01 ! NEUERSTELLUNG             ! PRIGGEN  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0085                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT-ALL <<<    PEWS TBPA0014                      *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA0085.
          05 PI-DRPA0085-PEWS-KEY.
             10 PI-DRPA0085-PGEB-GES         PIC S9(004) BINARY.
             10 PI-DRPA0085-PROD-PROD-SCHL   PIC S9(004) BINARY.
             10 PI-DRPA0085-PEWS-PROGN-DAT   PIC  X(010).
             10 PI-DRPA0085-PVSG-VSTAT-GRP   PIC S9(004) BINARY.
      *
       01 PO-DRPA0085.
          05 PO-DRPA0085-RC                  PIC  X(003).
             88 PO-DRPA0085-RC-OK                        VALUE 'OK '.
             88 PO-DRPA0085-RC-NOT-FOUND                 VALUE 'NFD'.
          05 PO-DRPA0085-PEWS-DATEN.
             10 PO-DRPA0085-PEWS-ANZ-ANTRAG  PIC S9(009) BINARY.
             10 PO-DRPA0085-PEWS-ANZ-VERTR-6 PIC S9(009) BINARY.
             10 PO-DRPA0085-PEWS-ANZ-VERTR-4 PIC S9(009) BINARY.
             10 PO-DRPA0085-PEWS-ANZ-VERTR-2 PIC S9(009) BINARY.
             10 PO-DRPA0085-PEWS-ANZ-VERTR-1 PIC S9(009) BINARY.
             10 PO-DRPA0085-PEWS-BETR-ANTR   PIC S9(009) BINARY.
             10 PO-DRPA0085-PEWS-BETR-VERTR  PIC S9(009) BINARY.
             10 PO-DRPA0085-PEWS-EINH-ANTR   PIC S9(009) BINARY.
             10 PO-DRPA0085-PEWS-EINH-VERTR  PIC S9(009) BINARY.
          05 PO-DRPA0085-PEWS-LAST-UPDATE    PIC  X(026).
          05 PO-DRPA0085-PEWS-KENNUNG        PIC  X(008).
      *
      ****** Ende Copy-Member ****************************************
