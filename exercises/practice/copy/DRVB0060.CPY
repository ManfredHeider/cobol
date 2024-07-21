      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0060.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB0060.CPv  $
      *
      *   Rev 2.2   Mar 02 2009 14:07:34   t13ex57
      *AAS00806: HOST-Schnittstellenerweiterung
      *
      *   Rev 2.1   Mar 11 2002 12:14:56   t13ae28
      *AVB00556: Ramsis-SST wg. System4 angepasst
      *
      *   Rev 2.0   Feb 27 1999 15:57:54   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:15:10   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 
      *    COPY-MEMBER: DRVB0060                                      * 
      *    --------------------------------------------------------   * 
      *    SATZBESCHREIBUNG DATEI ALV VB-ADRESSE                      * 
      *    FUER EINSTELLUNG IN DEN INFOPOOL                           *
      *    --------------------------------------------------------   * 
      *    ERSTELLT: BERGHOFER                                        * 
      *    DATUM:    28.06.1994                                       * 
      ***************************************************************** 
      *    VERSION   1    05.08.94 16:12:56
      ****************************************************************  
      * AENDERUNGEN:                                                 *  
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  
      * -------------------------------------------------------------*  
      * AFVB0123!28.06.94! 01 ! NEU ERSTELLT              ! BERGHOFER*  
      *         !        !    !                           !          *  
      * AVB00556!27.09.01! 06 ! ENTFERNEN DOPPELTE BANK-  ! D.EHRMANN*
      *         !        !    ! SÄTZE UND ENTFERNEN CR+LF !          *
      *         !        !    ! IM BANK-SATZ              !          *
270109* AAS00806!27.01.09!2.10! Tel/FAX v 14-> 26 Stellen ! U. Boll  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *  
      *         !        !    !                           !          *  
      *                                                              *  
      * ENDE-LOGBUCH                                                 *  
      ****************************************************************  
      *    DRVB0060-ST-NR               VB-NUMMER                    *  
      *    DRVB0060-ST-PRUEFZIFFER      VB-PRUEFZIFFER               *
      *    DRVB0060-KNR-ZUSTELLUNG      WIEVIELTE ADRESSE DES VB     *
      *    DRVB0060-MAN-NAME            NACHNAME                     *
      *    DRVB0060-MAN-VORNAME         VORNAME                      *
      *    DRVB0060-MAN-TITEL-ADEL      ADELSTITEL                   *
      *    DRVB0060-MAN-TITEL-AKAD      AKADEMISCHER TITEL           *
      *    DRVB0060-KNR-ANREDE          ANREDESCHLUESSEL             *
      *    DRVB0060-KNR-ANREDE-TEXT     HERR, FRAU, EHELEUTE ...     *
      *    DRVB0060-KNR-ANREDE-ERW1     ANREDEERWEITERUNG 1          *
      *    DRVB0060-KNR-ANREDE-ERW2     ANREDEERWEITERUNG 2          *
      *    DRVB0060-MAAN-STRASSE        STRASSE                      *
      *    DRVB0060-KNR-AUSL            AUSLANDSKENNZEICHEN          *
      *    DRVB0060-MAAN-PLZ-AKTUELL    PLZ FUER STRASSE             *
      *    DRVB0060-MAAN-ORT            WOHNORT                      *
      *    DRVB0060-MAAN-TELEFON        TELEFONNUMMER                *
      *    DRVB0060-MAAN-TELEFAX        TELEFAXNUMMER                *
      *    DRVB0060-MAAN-TYP            ADRESSART                    *
      *                                                              *  
      ****************************************************************  
       01  DRVB0060-SATZ.                                               
           05  DRVB0060-ST-NR                  PIC 9(007).
           05  DRVB0060-ST-PRUEFZIFFER         PIC X(001).
           05  DRVB0060-KNR-ZUSTELLUNG         PIC 9(002).
           05  DRVB0060-MAN-NAME               PIC X(040).
           05  DRVB0060-MAN-VORNAME            PIC X(020).
           05  DRVB0060-MAN-TITEL-ADEL         PIC X(010).
           05  DRVB0060-MAN-TITEL-AKAD         PIC X(010).
           05  DRVB0060-KNR-ANREDE             PIC 9(002).
           05  DRVB0060-KNR-ANREDE-TEXT        PIC X(015).
           05  DRVB0060-KNR-ANREDE-ERW1        PIC X(002).
           05  DRVB0060-KNR-ANREDE-ERW2        PIC X(002).
           05  DRVB0060-MAAN-STRASSE           PIC X(040).
           05  DRVB0060-KNR-AUSL               PIC X(004).
           05  DRVB0060-MAAN-PLZ-AKTUELL       PIC X(009).
           05  DRVB0060-MAAN-ORT               PIC X(040).
270109*    05  DRVB0060-MAAN-TELEFON           PIC X(014).
270109*    05  DRVB0060-MAAN-TELEFAX           PIC X(014).
270109     05  DRVB0060-MAAN-TELEFON           PIC X(026).
270109     05  DRVB0060-MAAN-TELEFAX           PIC X(026).
           05  DRVB0060-MAAN-TYP               PIC X(005).
270901*    05  DRVB0060-ZEILENENDE.
270901*        10 DRVB0060-CR                  PIC X(001).
270901*        10 DRVB0060-LF                  PIC X(001).
      *                                                                 
      ******** ENDE COPY-MEMBER (DRVB0060) *******************          
                                                                        
