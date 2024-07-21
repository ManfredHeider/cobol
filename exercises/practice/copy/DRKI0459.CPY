      * P13SP99   Stand: 27/09/98    16:34:32
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0459.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI0459.CPv  $
      *
      *   Rev 2.3   Jan 02 2008 17:30:46   t13ex48
      *AKL00666: Anpassung RUECK-DTA für Allfinanz PGM KUe
      *
      *   Rev 2.2   Apr 01 2005 12:10:24   t13ex48
      *AKI00460: e-Post: Bereitstellg. der tägl. Änd. im Betreuungs
      *verhältnis
      *
      *   Rev 2.1   Feb 24 2003 08:55:26   t13ex27
      *AKI00303 - Kommisarische Kundenübertragung
      *
      *   Rev 2.0   Apr 16 1999 13:20:22   T13EX19
      *aki00028
      *
      *   Rev 1.0   28 Jul 1998 09:55:58   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI0459  FUNKTION                           *
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
      * --------!--------!----!---------------------------!----------*
      * AKI00303!25.11.02! 02 ! Die Felder                ! W.MORFELD*
      *         !        !    ! KDUEBERTRAGUNGSART        !          *
      *         !        !    ! KDUGUELTIGABDAT           !          *
      *         !        !    ! neu augenommen            !          *
      * --------!--------!----!---------------------------!----------*
      * AKI00460!21.02.05! 03 ! Neues Feld VBalt-VBNR     ! HSE      *
      * --------!--------!----!---------------------------!----------*
      * AKL00666!19.12.07! 04 ! Neues Feld MANDANTNR      ! HSE      *
      * --------!--------!----!---------------------------!----------*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI0459                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  KUNDENBETREUERWECHSELDATEN ERMITTELN <<<             *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRKI0459.
          05 PI-DRKI0459-KEY.
             10 PI-DRKI0459-UFKT            PIC  X(002).
      *
       01 PO-DRKI0459.
          05 PO-DRKI0459-RC                 PIC  X(003).
             88 PO-DRKI0459-RC-OK                VALUE 'OK '.
             88 PO-DRKI0459-RC-LEER              VALUE 'LER'.
             88 PO-DRKI0459-RC-EOF               VALUE 'EOF'.
             88 PO-DRKI0459-RC-FEHLPROTOKOLL     VALUE 'FP '.
          05 PO-DRKI0459-DATEN.
             10 PO-DRKI0459-VERTRAGSID      PIC  X(013).
             10 PO-DRKI0459-VBALTID         PIC  X(013).
             10 PO-DRKI0459-VBALTROLLENKZ   PIC  X(001).
             10 PO-DRKI0459-VBNEUID         PIC  X(013).
             10 PO-DRKI0459-VBNEUROLLENKZ   PIC  X(001).
             10 PO-DRKI0459-DTAERLEDIGTKZ   PIC  X(001).
             10 PO-DRKI0459-LETZTEAENDERUNG PIC  X(026).
             10 PO-DRKI0459-aenderungskz    PIC  9(002).
             10 PO-DRKI0459-vertragsnr      PIC  9(015).
             10 PO-DRKI0459-direktionsnr    PIC  9(003).
             10 PO-DRKI0459-kundenbetreuernr
                                            PIC  9(007).
             10 PO-DRKI0459-PARTGESELLSCHAFTID
                                            PIC  9(004).
      *
      ***    ZUSATZDATEN FUER CKV
             10 PO-DRKI0459-INTVERTRAGSNR   PIC  9(007).
             10 PO-DRKI0459-PRUEFZIFFER     PIC  9(002).
      *
      ***    ZUSATZDATEN FUER KOKÜ
             10 PO-DRKI0459-KDUEBERTRAGUNGSART PIC  X(001).
             10 PO-DRKI0459-KDUGUELTIGABDAT    PIC  X(010).
      ***    ZUSATZDATEN FUER EPOST
210205       10 PO-DRKI0459-VBALT-VBNR      PIC  9(007).
191207***    ZUSATZDATEN FUER Mandanten-Trennung wg. ALLFINANZ
191207       10 PO-DRKI0459-MANDANTNR       PIC  9(007).
      *
      ****** Ende Copy-Member ****************************************
