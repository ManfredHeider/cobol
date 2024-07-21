      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21.09.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1063.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1063.CPv  $
      *
      *   Rev 2.1   Oct 12 2012 11:28:26   t13ae36
      *AAS01038: Erfolge im Gruppengeschäft - Jahresanfangsbestände
      *
      *   Rev 2.0   Sep 21 2012 10:43:04   t13ae36
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ***************************************************************** 
      *    COPY-MEMBER: DRVB1063                                      * 
      *    --------------------------------------------------------   * 
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0317                    * 
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION ÜBERGEBEN WERDEN    * 
      *    --------------------------------------------------------   * 
      *    ERSTELLT: -                                                * 
      *    DATUM:    21.09.2012                                       * 
      ***************************************************************** 
      * AENDERUNGEN:                                                  * 
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      * 
      * --------+--------+----+---------------------------+-----------* 
      * AAS01038!21.09.12!2.1 ! NEU                       ! R.Franke  *
      *         !        !    !                           !           * 
      * ENDE-LOGBUCH                                                  * 
      ***************************************************************** 
      *                                                               * 
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL          * 
      *    PRO FUNKTION ÜBERGEBEN WERDEN --> PI/PO-BEREICH            * 
      *                                                               * 
      *    FUNKTION DRVB1063 - ERMITTELN SATZ AUS TABELLE UEBJSEG     * 
      *                                                               * 
      ***************************************************************** 
                                                                        
       01 PI-DRVB1063.                                                  
          05 PI-DRVB1063-DATEN.                                         
101012       10 PI-DRVB1063-UEBJSEG-BETR-NR   PIC S9(09)  BINARY.
101012       10 PI-DRVB1063-UEBJSEG-MONAT     PIC  X(10).
                                                                        
       01 PO-DRVB1063.                                                  
         03 PO-DRVB1063-RC                     PIC  X(03).              
             88 PO-DRVB1063-RC-OK                   VALUE 'OK '.        
             88 PO-DRVB1063-RC-NOT-FOUND            VALUE 'NFD'.        
         03 PO-DRVB1063-DATEN.                                          
          05 PO-DRVB1063-DCL-VIVB0317.                                  
             10 PO-DRVB1063-UEBJSEG-BETR-NR    PIC S9(09)       BINARY. 
             10 PO-DRVB1063-UEBJSEG-MONAT      PIC  X(10).              
             10 PO-DRVB1063-UEBJSEG-BTRG-S-AKT PIC S9(09)V9(02) COMP-3.
             10 PO-DRVB1063-UEBJSEG-BTRG-S-ALT PIC S9(09)V9(02) COMP-3.
      *---------------------------------------------------------------* 
      *----   ENDE  COPY-MEMBER  DRVB1063   --------------------------* 
      *---------------------------------------------------------------* 
