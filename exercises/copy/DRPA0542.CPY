      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0542.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0542.CPv  $
      *
      *   Rev 2.1   May 27 1999 09:43:44   T13EX25
      *APA00042, Korr. V2.0
      *
      *
      *
      *   Rev 2.0   May 27 1999 08:58:10   T13EX25
      *APA00042, Erfassung V2.0
      *
      *   Rev 1.0   01 Oct 1997 11:28:40   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0542                                      
      *    --------------------------------------------------------   
      *    SCHNITTSTELLE FÜR DAS BUFFER-OZP ZYPA0010                  
      *    --------------------------------------------------------   
      *    ERSTELLT: BOHLINGER                                        
      *    DATUM:    21.10.1997                                       
      ****************************************************************
      *    VERSION                                                    
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0020!21.10.97! 01 ! NEU                       ! BOHLINGER*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                               
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL          
      *    PRO UPRO ÜBERGEBEN WERDEN.                                 
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN       
      *                (IN SEPARATEN COPYS PRO FUNKTION)              
      *                                                               
      ****************************************************************
      *                                                              
       01  PI-ZYPA0010.                                              
           05  PI-ZYPA0010-FUNKTION            PIC  X(008).          
               88 PI-ZYPA0010-DRPA0543              VALUE 'DRPA0543'.
      *                                                              
      ******* ENDE COPY-MEMBER **************************************