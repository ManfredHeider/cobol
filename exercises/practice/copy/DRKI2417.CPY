      *---------------------------------------------------------------* 
      *    Standard fuer alle Objekte unter PVCS VM                     
      *            Datum      :  19.08.2010                             
      *            Kategorie  :  Copystrecke                            
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH         
      *---------------------------------------------------------------* 
      *    $Workfile:   DRKI2417.CPY  $                                         

      *    $Revision:   2.1  $                                          
      *                                                                 
      *    $Log:   L:/mf/CPY/DRKI2417.CPv  $                                
      *
      *   Rev 2.1   Mar 30 2011 13:12:30   t13ex14
      *AKI01030: Anpassen Aufbereitung f. Advocard u. DMB
      *                                                                 
      *   Rev 2.0   Aug 19 2010 14:52:28   t13ex39                      
      *Archiverstellung                                                 
      *                                                                 
      *---------------------------------------------------------------* 
      ****************************************************************  
      *    COPY-MEMBER: DRKI2417 für SYSIN-Member KI121740           *
      *    ----------------------------------------------------------*  
      *    COPY-MEMBER FÜR DAS AUFBEREITUNGSPROGRAMM ADVOCARD/DMB
      *   - MAPPING PARTNER-WAGNIS-NR ZU DVAG-VERTRAGS-ID               
      *   - EINLESEN ÜBER VORLAUFKARTEN (80 BYTES)
      ****************************************************************  
      *    VERSION                                                      
      ****************************************************************  
      * AENDERUNGEN:                                                 *  
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  
      * -------------------------------------------------------------*  
      * AKI01030!29.03.11!    ! Neu                       ! WMI      *  
      *         !        !    !                           !          *  
      *                                                              *  
      * ENDE-LOGBUCH                                                 *  
      ****************************************************************  
      *******
      ****          Für Ges. 81 Advocard müssen fehlende Tarife
      ****          im SYSIN-Member KI121740 ergänzt werden.
      ****
      ****          Für Ges. 82 DMB müssen keine fehlende Tarife
      ****          ergänzt werden.
      *******
      *    Ursprüngliches Mapping im PMKI0205:
      *                                xxxxx           Vertragsartid
      *                                      xxxxx     Gelieferte Art
      *                                            xx  Partner
      *                                            xx  81=Advocard
      *                                            xx  82=DMB
      *    05  filler pic x(14) value '00025 00140 81'.
      *
       01  K-VTRART-ARR-ALL.
           05  K-VTRART-ARC-OCC        OCCURS 500.
               10   FILLER             PIC X(001).
               10   K-VTART-ID         PIC X(004).
               10   FILLER             PIC X(001).
               10   K-VTART-GEL        PIC 9(005).
               10   FILLER             PIC X(001).
               10   K-VTART-GES        PIC 9(002).
               10   FILLER             PIC X(066).

       01  K-WAGNIS-MAX                PIC 9(004) VALUE 500.
      *********************** Ende DRKI2417 **************************
