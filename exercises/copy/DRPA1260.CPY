      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1260.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1260.CPv  $
      *
      *   Rev 2.1   16 Jul 1999 11:02:02   T01JM2E
      *APA00012 - Ausgabesatz Rückmeldung an Antrag
      *
      *   Rev 2.0   Mar 30 1999 08:44:16   P13SP99
      *Neuerstellung
      *
      *   Rev 2.0   Mar 05 1999 10:18:46   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1260                                      *         
      *    --------------------------------------------------------   *
      *    AUSGABESATZ RÜCKMELDUNG AN ANTRAG PMPA0035                 *
      *    --------------------------------------------------------   *
      *    ERSTELLT: D.WILHELM                                        *
      *    DATUM:    08.06.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!08.06.99!2.1 ! NEU                       !D.WILHELM *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
          10 DRPA1260-DATEN.                                                    
            20 DRPA1260-GES               PIC S9(004)  USAGE COMP.
            20 DRPA1260-VNR               PIC  X(015).
            20 DRPA1260-KUNDE-NAME        PIC  X(025).
            20 DRPA1260-PROD-PROD-SCHL    PIC S9(004)  USAGE COMP.
            20 DRPA1260-PDPT-PRODART      PIC S9(004)  USAGE COMP.
            20 DRPA1260-PMPT-PPFL-BEITRAG PIC S9(009)V9(2) USAGE COMP-3.
            20 DRPA1260-PEST-AVERM        PIC  9(010).                          
            20 DRPA1260-EINH              PIC S9(6)V99 PACKED-DECIMAL.
            20 DRPA1260-BUCHDAT           PIC  9(8).
      *
      ******* ENDE COPY-MEMBER DRPA1260 *****************************           






























