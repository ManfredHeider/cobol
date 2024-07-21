      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0061.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB0061.CPv  $
      *
      *   Rev 2.2   Apr 15 2013 15:36:48   t13ex71
      *AVB01244: Projekt SEPA - Realisierung VB und AN - PMVB0109
      *
      *   Rev 2.1   Mar 11 2002 12:15:02   t13ae28
      *AVB00556: Ramsis-SST wg. System4 angepasst
      *
      *   Rev 2.0   Feb 27 1999 15:57:54   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:15:22   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 
      *    COPY-MEMBER: DRVB0061                                      * 
      *    --------------------------------------------------------   * 
      *    SATZBESCHREIBUNG DATEI ALV VB-BANK                         * 
      *    FUER EINSTELLUNG IN DEN INFOPOOL                           *
      *    --------------------------------------------------------   * 
      *    ERSTELLT: BERGHOFER                                        * 
      *    DATUM:    28.06.1994                                       * 
      ***************************************************************** 
      *    VERSION   1    05.08.94 16:13:38
      ****************************************************************  
      * AENDERUNGEN:                                                 *  
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  
      * -------------------------------------------------------------*  
      * AFVB0123!28.06.94! 01 ! NEU ERSTELLT              ! BERGHOFER*  
      * AVB00556!27.09.01! 02 ! DOPPELTE BANK-DATEN       ! D.EHRMANN*  
      *         !        !    ! ENTFERNEN                 !          *  
      * AVB01244!15.04.13! 03 ! IBAN u. BIC aufnehmen     ! B.Pretsch*  
      *         !        !    !                           !          *  
      *                                                              *  
      * ENDE-LOGBUCH                                                 *  
      ****************************************************************  
      *    DRVB0061-ST-NR               VB-NUMMER                    *  
      *    DRVB0061-ST-PRUEFZIFFER      VB-PRUEFZIFFER               *  
      *    DRVB0061-KNR-BV-TYP          BANKVERBINDUNGSNUMMER        *  
      *    DRVB0061-KNR-SPERRE          KONTOSTATUS-SCHLUESSEL       *  
      *    DRVB0061-KNR-SPERRE-TEXT     KONTOSTATUS-KURZTEXT         *  
      *    DRVB0061-BV-KTO-NR           KONTONUMMER                  *  
      *    DRVB0061-BV-KTO-INHABER      KONTOINHABER                 *  
      *    DRVB0061-BV-BANKNAME         NAME DER BANK                *  
      *    DRVB0061-BV-BLZ              BANKLEITZAHL                 *  
150413*    DRVB0061-BV-IBAN             IBAN                         *  
150413*    DRVB0061-BV-BIC              BIC                          *  
      *                                                              *  
      ****************************************************************  
       01  DRVB0061-SATZ.                                               
           05  DRVB0061-ST-NR                  PIC 9(007).
           05  DRVB0061-ST-PRUEFZIFFER         PIC X(001).
           05  DRVB0061-KNR-BV-TYP             PIC 9(002).
           05  DRVB0061-KNR-SPERRE             PIC 9(001).
           05  DRVB0061-KNR-SPERRE-TEXT        PIC X(040).
           05  DRVB0061-BV-KTO-NR              PIC X(020).
           05  DRVB0061-BV-KTO-INHABER         PIC X(040).
           05  DRVB0061-BV-BANKNAME            PIC X(040).
           05  DRVB0061-BV-BLZ                 PIC X(011).
150413     05  DRVB0061-BV-IBAN                PIC X(034).
150413     05  DRVB0061-BV-BIC                 PIC X(011).
270901*    05  DRVB0061-ZEILENENDE.
270901*        10  DRVB0061-CR                 PIC X(001).
270901*        10  DRVB0061-LF                 PIC X(001).
      *                                                                 
      ******** ENDE COPY-MEMBER (DRVB0061) *******************          
                                                                        
