      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0494.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA0494.CPv  $
      *
      *   Rev 2.3   Oct 30 2015 11:28:28   t13ae46
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: Dialog
      *e (PG11, PG12, PG14, PF11, PO22)
      *
      *   Rev 2.2   Aug 06 2015 09:17:34   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System, Anpass
      *ungen für PDIF
      *
      *   Rev 2.1   Jul 16 2015 15:17:18   t13ex67
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System, Anpass
      *ungen für PDIF
      *
      *   Rev 2.0   Feb 27 1999 14:29:06   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 09:14:26   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   23 Jul 1997 12:43:44   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0494                                      *
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
      *         !29.10,15!2.3 ! NEU: UMSATZSTEUERBETRAG   ! WANG     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0494                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0494.
          05 PI-DRPA0494-DATEN.
             10 PI-DRPA0494-PDTA-ID          PIC S9(015)  USAGE COMP-3.
      *
       01 PO-DRPA0494.
          05 PO-DRPA0494-RC                  PIC  X(003).
             88 PO-DRPA0494-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0494-RC-NOT-OK                     VALUE 'NOK'.
          05 PO-DRPA0494-DATEN.
             10 PO-DRPA0494-PDIF-ANZ-BEW-V     PIC S9(9)      COMP.
             10 PO-DRPA0494-PDIF-GUTSCHR-V     PIC S9(09)V99  COMP-3.
             10 PO-DRPA0494-PDIF-BELAST-V      PIC S9(09)V99  COMP-3.
160615*      10 PO-DRPA0494-PDIF-UST-BTR-GUT-V PIC S9(09)V99  COMP-3.
291015       10 PO-DRPA0494-PDIF-UST-GUT-V     PIC S9(09)V99  COMP-3.
291015       10 PO-DRPA0494-PDIF-UST-BEL-V     PIC S9(09)V99  COMP-3.


