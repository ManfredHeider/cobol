      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0495.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA0495.CPv  $
      *
      *   Rev 2.3   Oct 30 2015 11:28:38   t13ae46
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: Dialog
      *e (PG11, PG12, PG14, PF11, PO22)
      *
      *   Rev 2.2   Aug 06 2015 09:17:42   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System, Anpass
      *ungen für PDIF
      *
      *   Rev 2.1   Jul 16 2015 15:17:28   t13ex67
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System, Anpass
      *ungen für PDIF
      *
      *   Rev 2.0   Feb 27 1999 14:29:06   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 09:14:58   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   23 Jul 1997 12:43:50   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0495                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<LIST ZU PDIF  >>                         *
      *    --------------------------------------------------------   *
      *    ERSTELLT: D.WILHELM                                        *
      *    DATUM:    10.09.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!10.09.97!    ! NEU                       ! D.WILHELM*
      * APA01932!16.06.15!2.01! NEU:  UMSATZSTEUERBETRAG  ! STORCH   *
      *         !05.08.15!2.01! ÄNDERUNGEN WIEDER ZURÜCK  ! KÖPRÜCÜ  *
      * APA01932!29.10.15!2.3 ! NEU:  UMSATZSTEUERBETRAG  ! WANG     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0495                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0495.
          05 PI-DRPA0495-DATEN.
             10 PI-DRPA0495-PDTA-ID          PIC S9(015)  USAGE COMP-3.
      *
       01 PO-DRPA0495.
          05 PO-DRPA0495-RC                  PIC  X(003).
             88 PO-DRPA0495-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0495-RC-NOT-OK                     VALUE 'NOK'.
          05 PO-DRPA0495-DATEN.
             10 PO-DRPA0495-PDIF-ANZ-BEW-NV     PIC S9(9)     COMP.
             10 PO-DRPA0495-PDIF-GUTSCHR-NV     PIC S9(09)V99 COMP-3.
             10 PO-DRPA0495-PDIF-BELAST-NV      PIC S9(09)V99 COMP-3.
160615*      10 PO-DRPA0495-PDIF-UST-BETRAG-NV  PIC S9(09)V99 COMP-3.
291015       10 PO-DRPA0495-PDIF-UST-GUT-NV     PIC S9(09)V99 COMP-3.
291015       10 PO-DRPA0495-PDIF-UST-BEL-NV     PIC S9(09)V99 COMP-3.

