      *---------------------------------------------------------------* 
      *    Standard fuer alle Objekte unter PVCS VM                     
      *            Datum      :  23.11.2007                             
      *            Kategorie  :  Copystrecke                            
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH         
      *---------------------------------------------------------------* 
      *    $Workfile:   DRKI2215.CPY  $                                 00010000

      *    $Revision:   2.1  $                                          
      *                                                                 
      *    $Log:   L:/mf/CPY/DRKI2215.CPv  $                                
      *
      *   Rev 2.1   Dec 19 2007 13:56:08   t13ex50
      *AKI00639: PremiumDepot: Datenspiegel und Programmanpassungen
      *                                                                 
      *   Rev 2.0   Nov 23 2007 09:44:42   t13ex50                      
      *Archiverstellung                                                 
      *                                                                 
      *---------------------------------------------------------------* 
      ****************************************************************  
      *    COPY-MEMBER: DRKI2215                                   *    
      *    ----------------------------------------------------------*  
      *    SCHNITTSTELLE FÜR DAS UPRO ZWKI0314                     *    
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN       *  
      *    ----------------------------------------------------------*  
      ****************************************************************  
      *   erzeugt mit genOZP - (c) PMSmicado  20.11.2002/09:38:05       
      ****************************************************************  
      *    VERSION                                                      
      ****************************************************************  
      * AENDERUNGEN:                                                 *  
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  
      * -------------------------------------------------------------*  
      * AKI00639!23.11.07!2.0 ! NEUERSTELLUNG FÜR ZWKI0314!SCHLEICHER*  
      *         !        !    ! WEGEN 3 NEUER FELDER      !          *  
      *                                                              *  
      * ENDE-LOGBUCH                                                 *  
      ****************************************************************  
      *                                                                 
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL            
      *    PRO UPRO ÜBERGEBEN WERDEN.                                   
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN         
      *                (IN SEPARATEN COPYS PRO FUNKTION)                
      *                                                                 
      *    DRKI2216: DELETE                                             
      *    DRKI2216: INSERT                                             
      *    DRKI2216: UPDATE                                             
      *    DRKI2216: SELECT                                             
      *    DRKI2216: READ-MAX                                           
      *    DRKI2216: UPDATE (1)                                         
      *    DRKI2216: SELECT (1)                                         
      *                                                                 
      ****************************************************************  
      *                                                                 
       01  PI-ZWKI0314.                                                 
           05  PI-ZWKI0314-FUNKTION            PIC  X(008).             
                                                                        
                                                                        
             88   PI-ZWKI0314-DRKI2216     VALUE 'DRKI2216'.            
             88   PI-ZWKI0314-DRKI2217     VALUE 'DRKI2217'.            
             88   PI-ZWKI0314-DRKI2218     VALUE 'DRKI2218'.            
             88   PI-ZWKI0314-DRKI2219     VALUE 'DRKI2219'.            
             88   PI-ZWKI0314-DRKI2220     VALUE 'DRKI2220'.            
                                                                        
      *                                                                 
      ******* ENDE COPY-MEMBER **************************************   
      *---------------------------------------------------------------* 
