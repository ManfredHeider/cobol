      * P13SP99   Stand: 17/11/98    15:11:54
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0597.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI0597.CPv  $
      *
      *   Rev 2.2   Nov 19 2003 14:35:46   t13ex39
      *AKI00114.UE005 Rück-DTA
      *
      *   Rev 2.1   Nov 15 2002 15:36:34   t13ex58
      *AKI00305: Vertragsid eingefügt
      *
      *   Rev 2.0   Apr 16 1999 13:21:06   T13EX19
      *aki00028
      *
      *   Rev 1.0   07 Aug 1998 14:50:50   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI0597  FUNKTION                           *
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
      * AKI00305!21.10.02!    ! Feld VERVERTRAGSID eingef.! W.Morfeld*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI0597                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  VERTRAGS- UND VBDATEN ERMITTELN <<<                  *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRKI0597.
          05 PI-DRKI0597-KEY.
             10 PI-DRKI0597-UFKT               PIC  X(002).
          05 PI-DRKI0597-DATEN.
             10 PI-DRKI0597-ADAPERSONENID      PIC  X(013).
             10 PI-DRKI0597-ADAADRESSEALTID    PIC  X(013).
      *
      ***    SCHLUESSEL FUER UPDATE ERLEDIGTER RUECKDTASAETZE
             10 PI-DRKI0597-ADAADRESSENEUID    PIC  X(013).
      *
250903***    VERTRAGSID IST JETZT AUCH TEIL DES SCHLÜSSELS
250903       10 PI-DRKI0597-ADAVERTRAGSID      PIC  X(13).

      *
       01 PO-DRKI0597.
          05 PO-DRKI0597-RC                    PIC  X(003).
             88 PO-DRKI0597-RC-OK                 VALUE 'OK '.
             88 PO-DRKI0597-RC-LEER               VALUE 'LER'.
             88 PO-DRKI0597-RC-EOF                VALUE 'EOF'.
             88 PO-DRKI0597-RC-FP                 VALUE 'FP '.
          05 PO-DRKI0597-DATEN.
             10 PO-DRKI0597-vertragsnr         pic  9(015).
             10 PO-DRKI0597-kundenrolle        pic  X(001).
             10 PO-DRKI0597-kundenbetreuernr   pic  9(007).
             10 PO-DRKI0597-DIREKTIONSNR       pic  9(003).
             10 PO-DRKI0597-PARTGESELLSCHAFTID PIC  9(004).
990206*      10 PO-DRKI0597-GESCHAEFTSSTELLE   PIC S9(010).
990206       10 PO-DRKI0597-GESCHAEFTSSTELLE   PIC  9(003).
021021       10 PO-DRKI0597-VERVERTRAGSID      PIC  X(013).
      *
      ****** Ende Copy-Member ****************************************
