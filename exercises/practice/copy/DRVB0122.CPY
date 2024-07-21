      ***************************************************************** 
      *    --------------------------------------------------------   * 
      *    COPY-MEMBER: DRVB0122                                      * 
      *    --------------------------------------------------------   * 
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0012                        * 
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        * 
      *    --------------------------------------------------------   * 
      *    ERSTELLT: SCHÄFER                                          * 
      *    DATUM:    21.01.1997                                       * 
      ***************************************************************** 
      *    VERSION                                                      
      ****************************************************************  
      * AENDERUNGEN:                                                 *  
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  
      * -------------------------------------------------------------*  
      * AFPA0002!21.01.97! 01 ! NEU                       ! SCHÄFER  *  
      * AFVB0370!12.09.97! 02 ! Funktion -DRVB0164        ! Maderner *  
      * AAS00049!07.07.98! 03 ! NEUE FUNKTION DRVB0298    ! WALB     *  
      * AKI00001!16.07.98! 03 ! NEUE FUNKTION DRVB0254    ! Veit     *  
      * ASL00022!29.07.98!    ! NEUE FUNKTION DRVB0309    ! WALB     *  
      * ASL00022!24.08.98! 03 ! NEUE FUNKTION DRVB0312    ! WALB     *  
      * ASL00024!23.10.98! 04 ! NEUE FUNKTION DRVB0328    ! SCHMIDT,B*  
      * AAS00096!26.01.99! 05 ! NEUE FUNKTION DRVB0352    ! WALB     *  
      * AVB00445!04.06.99! 06 ! NEUE FUNKTION DRVB0376    ! SCHMIDT  *  
      * ASL00067!20.09.00! 07 ! NEUE FUNKTION DRVB0504    ! TH.WILLIG*  
      *         !        !    !   +  FUNKTION DRVB0505    !          *  
      *         !        !    !   +  FUNKTION DRVB0506    !          *  
      * AAS00264!07.03.01! 08 ! NEUE FUNKTION DRVB0527    ! WALB     *  
      * ACV00007!05.11.01! 09 ! NEUE FUNKTION DRVB0562,563! P.DUERR  *  
      * AVB00636!18.10.02! 10 ! NEUE FUNKTION DRVB0600    ! U.BOLL   *  
      * AVB00648!18.03.03! 11 ! NEUE FUNKTION DRVB0646    ! MUND     *
      * AVB00666!23.05.03! 12 ! NEUE FUNKTION DRVB0659    ! MUND     *
220904* AVD00001!22.09.04! 2.8! NEUE FUNKTION DRVB0741    ! KLEEMEIER*
010606* AVB00850!01.06.06! 2.9! NEUE FUNKTION DRVB0825    ! KLEEMEIER*
090606* ASL00224!09.06.06!2.10! NEUE FUNKTION DRVB0827    ! WALB     *
031108* AVB01007!03.11.08!2.11! NEUE FUNKTION DRVB0922    ! J.Hering *
120509* AVB01035!12.05.09!2.12! NEUE FUNKTION DRVB0940    ! U.Boll   *
      *                                                              *
260422* JIRA     ! DATUM  ! AENDERUNG                     ! NAME     *
260422* -------------------------------------------------------------*
260422* VLA-4642 !26.04.22! DRVB1340 HINZU UND PVCS-KOPF  !          *
260422*          !        ! ENTFERNT                      ! KLEEMEIER*
      *                                                              *
      *                                                              *  
      ****************************************************************  
      *                                                                 
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL            
      *    PRO UPRO ÜBERGEBEN WERDEN.                                   
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN         
      *                (IN SEPARATEN COPYS PRO FUNKTION)                
      *                                                                 
      ****************************************************************  
      *                                                                 
       01  PI-ZWVB0012.                                                 
           05  PI-ZWVB0012-FUNKTION            PIC  X(008).             
               88 PI-ZWVB0012-DRVB0123              VALUE 'DRVB0123'.   
120997         88 PI-ZWVB0012-DRVB0164              VALUE 'DRVB0164'.   
070798         88 PI-ZWVB0012-DRVB0298              VALUE 'DRVB0298'.   
160798         88 PI-ZWVB0012-DRVB0254              VALUE 'DRVB0254'.   
290798         88 PI-ZWVB0012-DRVB0309              VALUE 'DRVB0309'.   
240898         88 PI-ZWVB0012-DRVB0312              VALUE 'DRVB0312'.   
231098         88 PI-ZWVB0012-DRVB0328              VALUE 'DRVB0328'.   
260199         88 PI-ZWVB0012-DRVB0352              VALUE 'DRVB0352'.   
040699         88 PI-ZWVB0012-DRVB0376              VALUE 'DRVB0376'.   
200900         88 PI-ZWVB0012-DRVB0504              VALUE 'DRVB0504'.   
200900         88 PI-ZWVB0012-DRVB0505              VALUE 'DRVB0505'.   
200900         88 PI-ZWVB0012-DRVB0506              VALUE 'DRVB0506'.   
070301         88 PI-ZWVB0012-DRVB0527              VALUE 'DRVB0527'.   
051101         88 PI-ZWVB0012-DRVB0562              VALUE 'DRVB0562'.   
051101         88 PI-ZWVB0012-DRVB0563              VALUE 'DRVB0563'.   
181002         88 PI-ZWVB0012-DRVB0600              VALUE 'DRVB0600'.
180303         88 PI-ZWVB0012-DRVB0646              VALUE 'DRVB0646'.
230503         88 PI-ZWVB0012-DRVB0659              VALUE 'DRVB0659'.
220904         88 PI-ZWVB0012-DRVB0741              VALUE 'DRVB0741'.
010606         88 PI-ZWVB0012-DRVB0825              VALUE 'DRVB0825'.
090606         88 PI-ZWVB0012-DRVB0827              VALUE 'DRVB0827'.
031108         88 PI-ZWVB0012-DRVB0922              VALUE 'DRVB0922'.
120509         88 PI-ZWVB0012-DRVB0940              VALUE 'DRVB0940'.
260422         88 PI-ZWVB0012-DRVB1340              VALUE 'DRVB1340'.
      *
      ******* ENDE COPY-MEMBER **************************************
