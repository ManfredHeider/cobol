      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0030.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB0030.CPv  $
      *
      *   Rev 2.2   May 20 2008 12:13:12   t13ae48
      *AVB00875: Verlängerung der Adressfelder und Kontaktdaten - P
      *rogramme (JMS) - 3
      *
      *   Rev 2.1   Jan 17 2003 14:47:28   t13ae28
      *AVB00649: Dim 7ukunft - Neue Bef.Bed. RGS - D
      *
      *   Rev 2.0   Feb 27 1999 15:57:46   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:08:50   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 
      *    COPY-MEMBER: DRVB0030                                      * 
      *    AUFBAU TS-QUEUE (DATENTEIL)                                * 
      *    --------------------------------------------------------   * 
      *    ERSTELLT: HECKER                                           * 
      *    DATUM:    24.04.1993                                       * 
      ***************************************************************** 
      *    VERSION   02   16.09.93                                   *  
      ****************************************************************  
      * AENDERUNGEN:                                                 *  
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  
      * -------------------------------------------------------------*  
      *         !24.04.93! 01 ! NEU                       ! HECKER   *  
      *         !10.09.93! 02 !...30-DATEN 100 BYTE GRÖßER! SCHOELLH.*  
      *         !        !    !                           !          *  
      * AVB00649!16.12.02! 03 !TUPEL-LÄNGE MA GEÄNDERT    ! D.EHRMANN*
      *         !        !    !                           !          *
      *         !        !    !                           !          *  
      *         !        !    !                           !          *  
      *         !        !    !                           !          *  
      *                                                              *  
      * ENDE-LOGBUCH                                                 *  
      ****************************************************************  
      *                                                                 
       01  Z-DRVB0030-TSQ-DATEN.                                        
           05 Z-DRVB0030-TAB-NAME              PIC  X(004).             
           05 Z-DRVB0030-TIMESTAMP             PIC  X(026).             
           05 Z-DRVB0030-VERARB-KZ             PIC  X(001).             
161202*    05 Z-DRVB0030-DATEN                 PIC  X(269).             
161202     05 Z-DRVB0030-DATEN                 PIC  X(281).             
      *                                                                 
      *                                                                 
      ******** ENDE COPY-MEMBER (DRVB0030) *******************          
