      *---------------------------------------------------------------*         
      *    Standard fuer alle Objekte unter PVCS VM                             
      *            Datum      :  09.02.2021                                     
      *            Kategorie  :  Copystrecke                                    
      *---------------------------------------------------------------*         
      *                                                                         
      *---------------------------------------------------------------*         
      *    Copyright (c) 2021 ATLAS Dienstleistungen f. V. GmbH                 
      *---------------------------------------------------------------*         
      *    $Workfile:   DRKI3858.CPY  $                                         
      *    $Revision:   2.1  $                                                  
      *                                                                         
      *    $Log:   L:/mf/CPY/DRKI3858.CPv  $                                    
      *                                                                         
      *   Rev 2.1   Feb 28 2021 10:44:10   t13ex39                              
      *AKI01790: Deutsche Bank - SPARPLÄNE KSDS--->TT                           
      *                                                                         
      *   Rev 2.0   Feb 9 2021 06:30:47 T13EX39                                 
      *Archiverstellung                                                         
      *                                                                         
      *---------------------------------------------------------------*         
      *****************************************************************         
      *    COPY-MEMBER: DRKI3858                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3858                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         26.04.2021 / 11:00:09 / T13EX39            
      *****************************************************************         
      *    VERSION                                                              
      ****************************************************************          
      * AENDERUNGEN:                                                 *          
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *          
      * -------------------------------------------------------------*          
      *         !        !    !                           !          *          
      *         !        !    !                           !          *          
      *                                                              *          
      * ENDE-LOGBUCH                                                 *          
      ****************************************************************          
      *                                                                         
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER                    
      *    FÜR DIE FUNKTION DRKI3858                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  INSERT  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI3858.                                                         
           05  PI-DRKI3858-KEY.                                                 
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI3858-G48SPPLANNR   PIC  X(0002).                         
             10 PI-DRKI3858-G48SPVNR   PIC  X(0015).                            
             10 PI-DRKI3858-G48SPWAEHRUNGSKZ   PIC  X(0003).                    
END   *                                                                         
                                                                                
           05  PI-DRKI3858-NI.                                                  
                                                                                
ANF   *        %M_PI_DATEN_NI%                                                  
             15 FILLER                          PIC S9(4) COMP.                 
END   *                                                                         
                                                                                
           05  PI-DRKI3858-DATEN.                                               
                                                                                
ANF   *        %M_PI_DATEN%                                                     
             10 PI-DRKI3858-G48SPABWEICHKTOINH   PIC  X(0027).                  
             10 PI-DRKI3858-G48SPAUSGAKTFOND   PIC S9(2)V9(2) COMP-3.           
             10 PI-DRKI3858-G48SPAUSGFOND   PIC S9(2)V9(2) COMP-3.              
             10 PI-DRKI3858-G48SPBEGINNDYN   PIC  X(0008).                      
             10 PI-DRKI3858-G48SPBEGRATENAUS   PIC  X(0008).                    
             10 PI-DRKI3858-G48SPBIC   PIC  X(0011).                            
             10 PI-DRKI3858-G48SPBONIFIKATION   PIC S9(3)V9(4) COMP-3.          
             10 PI-DRKI3858-G48SPDATVERTKUEND   PIC  X(0008).                   
             10 PI-DRKI3858-G48SPDELKZ   PIC  X(0001).                          
             10 PI-DRKI3858-G48SPDEPOTUNTERNR   PIC  X(0002).                   
             10 PI-DRKI3858-G48SPENDERATENAUS   PIC  X(0008).                   
             10 PI-DRKI3858-G48SPIBAN   PIC  X(0034).                           
             10 PI-DRKI3858-G48SPISIN   PIC  X(0012).                           
             10 PI-DRKI3858-G48SPKOSTENPROZENT   PIC S9(1)V9(2) COMP-3.         
             10 PI-DRKI3858-G48SPKUENDMERKMAL   PIC  X(0001).                   
             10 PI-DRKI3858-G48SPMANDAT   PIC  X(0035).                         
             10 PI-DRKI3858-G48SPOFFNUNGSDATUM   PIC  X(0008).                  
             10 PI-DRKI3858-G48SPPIN   PIC S9(9)V COMP-3.                       
             10 PI-DRKI3858-G48SPPROZENTDYN   PIC S9(2)V9(2) COMP-3.            
             10 PI-DRKI3858-G48SPSCHLIESSDATUM   PIC  X(0008).                  
             10 PI-DRKI3858-G48SPSPARPLENDEVL   PIC  X(0008).                   
             10 PI-DRKI3858-G48SPSPARRATE   PIC S9(7)V9(2) COMP-3.              
             10 PI-DRKI3858-G48SPTAGEINZAHLUNG   PIC S9(4) COMP.                
             10 PI-DRKI3858-G48SPURGESAMTEURO   PIC S9(6)V9(2) COMP-3.          
             10 PI-DRKI3858-G48SPURSPARRATE   PIC S9(4)V9(2) COMP-3.            
             10 PI-DRKI3858-G48SPURSPRVERTNR   PIC  X(0012).                    
             10 PI-DRKI3858-G48SPVERTRAGLFZ   PIC S9(4) COMP.                   
             10 PI-DRKI3858-G48SPVERTRAGSTYP   PIC  X(0002).                    
             10 PI-DRKI3858-G48SPWKN   PIC  X(0006).                            
             10 PI-DRKI3858-G48SPZAHLWEISE   PIC  X(0001).                      
             10 PI-DRKI3858-G48SPZLASTRUECK   PIC S9(4) COMP.                   
END   *                                                                         
                                                                                
      *  INITIALISIERUNG NULL-INDIKATOR                                         
       01  PI-DRKI3858-NI1.                                                     
           05 PI-DRKI3858-INIT-TAB1 PIC S9(04) COMP VALUE -1                    
                                      OCCURS 1.                                 
       01  PI-DRKI3858-NI7.                                                     
           05 PI-DRKI3858-INIT-TAB7 PIC S9(04) COMP VALUE -7                    
                                      OCCURS 1.                                 
                                                                                
      *                                                                         
       01  PO-DRKI3858.                                                         
           05  PO-DRKI3858-RC                     PIC  X(003).                  
               88 PO-DRKI3858-RC-OK                  VALUE 'OK '.               
               88 PO-DRKI3858-RC-DUPLICATE-VALUE VALUE 'DVA'.                   
      *                                                                         
      ******* ENDE COPY-MEMBER **************************************           
