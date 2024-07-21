      * P13SP99   Stand: 12/10/98    14:29:41
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0536.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0536.CPv  $
      *
      *   Rev 2.0   Apr 16 1999 13:16:40   T13EX19
      *aki00028
      *
      *   Rev 1.0   07 Aug 1998 14:46:08   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI0536 FUNKTION:VERTRAGSDATEN ERMITTELN    *
      *                                   LIST                       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BRENNECKE                                       *
      *    DATUM:                                                    *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00006!17.08.98! 01 !NEU                        !BRENNECKE *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI0536                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    FUNKTION: VERTRAGSDATENERMITTELN LIST                     *
      *              - ALLE VERTRAEGE BEST.ROLLEN VON ADAPERSONENID  *
      *                ERMITTELN                                     *
      *              - MITARBEITERNR DES EINZELNEN VERTRAGES         *
      *                ERMITTELN                                     *
      *              - PARTNERGESELLSCHAFT ERMITTELN                 *
      ****************************************************************
       01 PI-DRKI0536.
           05  PI-DRKI0536-UFKT                 PIC  X(002).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST-NEXT, 3=QUIT
           05  PI-DRKI0536-DATEN.
             10  PI-DRKI0536-ADAPERSONENID        PIC  X(013).
             10  PI-DRKI0536-ADAADRESSEALTID      PIC  X(013).

      ***
       01 PO-DRKI0536.
          05 PO-DRKI0536-RC                     PIC  X(003).
             88 PO-DRKI0536-RC-OK                        VALUE 'OK '.
             88 PO-DRKI0536-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI0536-RC-EOF                       VALUE 'EOF'.
             88 PO-DRKI0536-RC-FP                        VALUE 'FP '.
          05 PO-DRKI0536-DATEN.
             10 PO-DRKI0536-VRBPERSONENROLLEKZ    PIC  X(001).
             10 PO-DRKI0536-VERVERTRAGSNUMMER     PIC  X(015).
             10 PO-DRKI0536-VERVERTRAGSID         PIC  X(013).
             10 PO-DRKI0536-VERPARTGESELLSCHID    PIC  9(004).
      *
      ******** Ende Copy-Member **************************************
