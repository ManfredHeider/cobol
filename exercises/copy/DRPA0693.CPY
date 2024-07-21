      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0693.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0693.CPv  $
      *
      *   Rev 2.0   Mar 10 1999 12:11:58   T13EX22
      *MFE-Umstellung
      *
      *   Rev 1.0   11 Nov 1997 17:00:40   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0693  PRINTE IN PVPT SUCHEN              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: MUND                                            *
      *    DATUM:    16.12.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!16.12.97! 01 ! NEUERSTELLUNG             ! MUND     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0693                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  PRINTE SUCHEN  <<<    TBPA0111 PVPT                  *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA0693.
          05 PI-DRPA0693-UFKT-CODE         PIC  9(002).
          05 PI-DRPA0693-DATEN.
               10 PI-DRPA0693-PIN          PIC S9(9) USAGE COMP.
      *
       01 PO-DRPA0693.
          05 PO-DRPA0693-RC                PIC  X(003).
             88 PO-DRPA0693-RC-OK               VALUE 'OK '.
             88 PO-DRPA0693-RC-LEER             VALUE 'LER'.
             88 PO-DRPA0693-RC-EOF              VALUE 'EOF'.
           05  PO-DRPA0693-DATEN.
               10 PO-DRPA0693-PRINTE       PIC S9(15)V USAGE COMP-3.
               10 PO-DRPA0693-PGEB-GES     PIC S9(04) BINARY.
      *
      ******* ENDE COPY-MEMBER **************************************
