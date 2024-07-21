      * P13SP99   Stand: 20/11/98    11:09:11
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0613.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0613.CPv  $
      *
      *   Rev 2.0   Apr 16 1999 13:21:22   T13EX19
      *aki00028
      *
      *   Rev 1.0   25 Aug 1998 09:23:26   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI0613  FUNKTION                           *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BRENNECKE                                       *
      *    DATUM:    19.08.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00006!19.08.98! 01 ! NEUERSTELLUNG             ! BRENNECKE*
      * Fehler  !06.02.99!    ! Geschaeftsst.-ID neu def. ! HSE      *
      *         !        !    ! von 10 auf 3 Stellen      !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI0613                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  VERTRAGS- UND VBDATEN ERMITTELN FUER                 *
      *         PERSNAMENAENDERUNG ERMITTELN         <<<             *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRKI0613.
          05 PI-DRKI0613-KEY.
             10 PI-DRKI0613-UFKT               PIC  X(002).
          05 PI-DRKI0613-DATEN.
             10 PI-DRKI0613-PNAPERSONENID      PIC  X(013).
      *
      ***    KEYDATEN ZUM DELETE BEI ERFOLGREICHER VERARBEITUNG
             10 PI-DRKI0613-PNAMITARBEITERID   PIC  X(013).
             10 PI-DRKI0613-PNAMITARBPERSROLLE PIC  X(001).
             10 PI-DRKI0613-PNAAENDERUNGSGRUND PIC  9(002).

      *
       01 PO-DRKI0613.
          05 PO-DRKI0613-RC                    PIC  X(003).
             88 PO-DRKI0613-RC-OK                 VALUE 'OK '.
             88 PO-DRKI0613-RC-LEER               VALUE 'LER'.
             88 PO-DRKI0613-RC-EOF                VALUE 'EOF'.
             88 PO-DRKI0613-RC-FP                 VALUE 'FP '.
          05 PO-DRKI0613-DATEN.
             10 PO-DRKI0613-vertragsnr         pic  9(015).
             10 PO-DRKI0613-kundenrolle        pic  X(001).
             10 PO-DRKI0613-kundenbetreuernr   pic  9(007).
             10 PO-DRKI0613-DIREKTIONSNR       pic  9(003).
             10 PO-DRKI0613-PARTGESELLSCHAFTID PIC  9(004).
             10 PO-DRKI0613-HERKUNFTSKZ        PIC  X(001).
             10 PO-DRKI0613-STRASSEALT         PIC  X(045).
             10 PO-DRKI0613-STRASSENEU         PIC  X(045).
             10 PO-DRKI0613-PLZALT             PIC  X(015).
             10 PO-DRKI0613-PLZNEU             PIC  X(015).
             10 PO-DRKI0613-ORTALT             PIC  X(030).
             10 PO-DRKI0613-ORTNEU             PIC  X(030).
             10 PO-DRKI0613-AUSLANDSKZALT      PIC  X(003).
             10 PO-DRKI0613-AUSLANDSKZNEU      PIC  X(003).
990206*      10 PO-DRKI0613-GESCHAEFTSSTELLE   PIC  9(010).
990206       10 PO-DRKI0613-GESCHAEFTSSTELLE   PIC  9(003).
      *
      ***
             10 PO-DRKI0613-ADRESSEID          PIC  X(013).
      *
      ****** Ende Copy-Member ****************************************
